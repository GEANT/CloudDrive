package providers.pithosplus

import net.vrijheid.clouddrive.control.{Storage, RootContext}
import net.vrijheid.clouddrive.httpsupport.MKCOL
import net.vrijheid.clouddrive.sharing.Sharing
import java.io.{FileInputStream, FileOutputStream, OutputStream, InputStream, File}
import org.slf4j.LoggerFactory
import gr.grnet.pithosj.core.ConnectionInfo

object PithosPlusFileSystem {
  /**
   * The shared HTTP Pithos+ client to be used by CloudDrive.
   */
  final val client = gr.grnet.pithosj.core.PithosClientFactory.newPithosClient()

  /**
   * Provides per-user configuration keys.
   * Use [[net.vrijheid.clouddrive.setup.VMSetup#addUser()]] to provide values for these keys
   * when adding the user to CloudDrive.
   */
  object Keys {
    /**
     * The user secret key. Pithos+ requires this instead of a password, so that the password remains
     * unknown to services. Note that this secret key must be renewed periodically, so a procedure
     * must be set-up within CloudDrive to support this feature.
     */
    final val UserToken = "pithos-user-token"

    /**
     * The public URL of the Pithos+ service.
     */
    final val ServerURL = "pithos-server-url"

    /**
     * The Pithos+ container used to store the files.
     */
    final val Container = "pithos-container"
  }

  /**
   * The default Pithos+ container used to store files if nothing else has been specifically configured.
   */
  final val DefaultContainer = "pithos"

  /**
   * Same role as `buffersize` in [[net.vrijheid.clouddrive.providers.aws.AWSFileSystem]].
   */
  final val ReadBufferSize = 1024 * 4

  final val EmptyByteArray = new Array[Byte](0)

  final val ReadMode = 'read
  final val WriteMode = 'write
  final val IHaveNoIdeaMode = 'ihavenoidea

  object Helpers {
    def isCollectionVerb(ctx: RootContext[_]): Boolean = {
      ctx.verb match {
        case verb: MKCOL => true
        case _ => false
      }
    }

    def newTmpFile(): File = {
      File.createTempFile(Integer.toHexString(System.identityHashCode(this)), ".pj")
    }
  }
}

// TODO: Remove this when the implementation is done.
// This is just an aid in debugging.
object FSSCounter {
  @volatile private var _counter = 0

  def nextValue = {
    _counter += 1
    _counter
  }
}

/**
 * Implements Pithos+ as a storage provider for CloudDrive.
 * The implementation is loosely modelled after [[net.vrijheid.clouddrive.providers.aws.AWSFileSystem]]
 * and [[net.vrijheid.clouddrive.providers.filesystem.FileSystemStore]], which are state machines in
 * disguise.
 *
 * @param filepath
 * @param ctx
 * @tparam T
 */
class PithosPlusFileSystem[T](filepath : String)(implicit ctx : RootContext[T]) extends Sharing[T] with Storage {
  import PithosPlusFileSystem._

  private[this] val logger = LoggerFactory.getLogger(this.getClass)
  logger.debug("### %s #############################".format(FSSCounter.nextValue))

  logger.debug("filepath = %s".format(filepath))

  /**
   * This is the actual user for the Pithos+ REST calls.
   */
  val userID = ctx.user

  /**
   * The user access token for the Pithos+ REST calls.
   */
  private val userToken = ctx.userConfig.get(Keys.UserToken)

  /**
   * The public Pithos+ service URL.
   */
  val serverURL = ctx.userConfig.get(Keys.ServerURL)

  /**
   * The Pithos+ container to be used for this `userID`.
   */
  private val container = ctx.userConfig.get(Keys.Container, DefaultContainer)

  /**
   * Connection information to be used by the Pithos+ client library.
   */
  private val connInfo = new ConnectionInfo(serverURL, userID, userToken)

  logger.debug("server=%s, container=%s, userID=%s".format(serverURL, container, userID))

  // From AWSFileSystem & LocalFileSystem logic
  val user = "/" + userID

  // Following AWSFileSystem & LocalFileSystem in the interpretation of this thing...
  val resolvedFilePath = filepath.startsWith(user) match {
    case true =>
      // following link for actual user
      followLink(stripTrailingSlash(filepath))

    case false if sharedSource(filepath) =>
      // shared resource
      followLink(stripTrailingSlash(filepath))

    case _ =>
      // ??
      followLink(stripTrailingSlash(user + filepath))
  }

//  val remoteFilePath = resolvedFilePath.startsWith(user) match {
//    case true =>
//      resolvedFilePath.substring(user.length)
//
//    case false =>
//      resolvedFilePath
//  }
  val remoteFilePath = stripLeadingSlash(filepath)

  logger.debug("resolvedFilePath = %s, remoteFilePath = %s".format(resolvedFilePath, remoteFilePath))

  // Apparently these are the API's way to communicate content to API's clients.
  // See for example [[net.vrijheid.clouddrive.pipes.webdavcmds.GETSink]].
  var input : InputStream = _
  var output : OutputStream = _

  // This var is needed because close() has no idea of the mode of the previous open()
  private var _prevOpenMode = IHaveNoIdeaMode
  private var _tmpReadFile: File = _
  private var _tmpWriteFile: File = _
  // I need this since I detected that close() is called multiple times (actually two)
  private var _isClosed = false

  // Same purpose as cleanup_on_write in AWSFileSystem
  private var _close_post_processor: () => Unit = _
  private def doClosePostProcessing() {
    if(_close_post_processor ne null) {
      _close_post_processor.apply()
      _close_post_processor = null
    }
  }

  /**
   * Downloads the file from Pithos+ and sets the local input stream accordingly.
   */
  private def openForRead() {
    _tmpReadFile = Helpers.newTmpFile()
    logger.debug("Created empty %s for %s".format(_tmpReadFile.getAbsolutePath, remoteFilePath))
    _close_post_processor = () => {
      _tmpReadFile.delete()
      logger.debug("Deleted %s for %s".format(_tmpReadFile.getAbsolutePath, remoteFilePath))
      _tmpReadFile = null
    }

    input = null
    val tmpOutputStream = new FileOutputStream(_tmpReadFile)
    val future = client.getObject(connInfo, container, remoteFilePath, null, tmpOutputStream)
    try {
      future.get()
      input = new FileInputStream(_tmpReadFile)
    }
    finally {
      tmpOutputStream.close()
    }
  }

  private def openForWrite() {
    create()
    setOriginal(resolvedFilePath, resolvedFilePath)
    logger.debug("setOriginal(%s, %s)".format(resolvedFilePath, resolvedFilePath))
  }

  def open(mode: Symbol) = {
    logger.debug("open(%s)".format(mode))
    this._prevOpenMode = mode

    val result = mode match {
      case ReadMode =>
        openForRead()
        true

      case WriteMode =>
        openForWrite()
        true

      case _ =>
        false
    }

    _isClosed = false
    result
  }

  private def doReadClose() {
    input.close()
    doClosePostProcessing()
  }

  private def doWriteClose() {
    output.flush()
    output.close()
    doClosePostProcessing()
  }

  def close() {
    logger.debug("[_isClosed=%s]".format(_isClosed))
    if(_isClosed) {
      return
    }

    try {
      this._prevOpenMode match {
        case ReadMode =>
          doReadClose()

        case WriteMode =>
          doWriteClose()

        case mode =>
          logger.info("[Unknown mode = %s]".format(mode))
      }
    }
    finally {
      _isClosed = true
      _prevOpenMode = IHaveNoIdeaMode
    }
  }

  def read(): Array[Byte] = {
    if(input ne null) {
      val buffer = new Array[Byte](ReadBufferSize)

      input.read(buffer) match {
        case -1 =>
          logger.debug("read() => [length=-1]")
          EmptyByteArray

        case n =>
          logger.debug("read() => [length=%s]".format(n))
          buffer.slice(0, n)
      }
    }
    else {
      logger.debug("read() [<no input>] => EmptyByteArray")
      EmptyByteArray
    }
  }

  def write(data: Array[Byte]) {
    val description = if(data eq null) "<null>" else "length=%s".format(data.length.toString)
    logger.debug("write(%s)".format(description))

    if((null ne output) && (null ne data)) {
      output.write(data)
    }
  }

  def copy() = {
    logger.debug("copy()")
    null
  }

  def getMetaData() = {
    val result = getMetaData(resolvedFilePath)
    logger.debug("getMetaData() => %s".format(result))
    result
  }

  // NOTE: This is called on PUT
  def transfer() {
    logger.debug("transfer()")
    if(output ne null) {
      output.flush()
      logger.debug("Transfering from %s to remote %s".format(_tmpWriteFile, remoteFilePath))
      val future = client.putObject(connInfo, container, remoteFilePath, _tmpWriteFile, "application/binary")
      val result = future.get()
      logger.debug("Transferred, result = %s".format(result))
      logger.debug("result.statusCode = %s, result.statusText = %s".format(result.statusCode, result.statusText))
      output = null
      if(!result.is201) {
        throw new Exception(
          "file = %s, result.statusCode = %s, result.statusText = %s".format(
            remoteFilePath,
            result.statusCode,
            result.statusText))
      }
    }
  }

  def transferLocalFile(src: File, newuuid: String) {
    logger.debug("transferLocalFile(%s, %s)".format(src.getCanonicalPath, newuuid))
  }

  def setMetaData(metadata: Map[String, String]) {
    setMetaData(resolvedFilePath, metadata)
    logger.debug("setMetaData(%s)".format(metadata))
  }

  def hasChildren() = {
    val result = hasChildren(resolvedFilePath)
    logger.debug("hasChildren() => %s", result)
    result
  }

  // This seems to be used for open('write)
  def create() = {
    if(Helpers.isCollectionVerb(ctx)) {
      logger.debug("Create collection %s (%s)".format(resolvedFilePath, remoteFilePath))
      createCollection(resolvedFilePath)
    }
    else {
      logger.debug("Create file %s (%s)".format(resolvedFilePath, remoteFilePath))
      createResource(resolvedFilePath)
      _tmpWriteFile = Helpers.newTmpFile()
      output = new FileOutputStream(_tmpWriteFile)
      logger.debug("Created output=_tmpWriteFile in %s".format(_tmpWriteFile))
      _close_post_processor = () ⇒ {
        _tmpWriteFile.delete()
        logger.debug("Deleted _tmpWriteFile %s for %s".format(_tmpWriteFile.getAbsolutePath, resolvedFilePath))
        _tmpWriteFile = null
      }
    }
    true
  }

  def deleteDirectly(uuid: String) {
    logger.debug("uuid = %s".format(uuid))
  }

  def delete() {
    if(exists()) {
      if(isCollection()) {
        // TODO: Delete the remote folder?
      }
      else {
        val future = client.deleteObject(connInfo, container, remoteFilePath)
        val result = future.get()
        logger.debug("Transferred, result = %s".format(result))
        logger.debug("result.statusCode = %s, result.statusText = %s".format(result.statusCode, result.statusText))
        if(!result.is204) {
          throw new Exception(
            "file = %s, result.statusCode = %s, result.statusText = %s".format(
              remoteFilePath,
              result.statusCode,
              result.statusText))
        }
      }
    }
  }

  def exists() = {
    val result = exists(resolvedFilePath)
    logger.debug("=> %s".format(result))
    result
  }

  def isCollection() = {
    val result = isCollection(resolvedFilePath)
    logger.debug("=> %s".format(result))
    result
  }

  def getOriginalName() = {
    val original = remoteFilePath
    logger.debug("=> %s".format(original))
    original
  }
}
