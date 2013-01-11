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
  final val DefaultContainer = "terena"

  /**
   * Same role as `buffersize` in [[net.vrijheid.clouddrive.providers.aws.AWSFileSystem]].
   */
  final val ReadBufferSize = 1024 * 4

  final val EmptyByteArray = new Array[Byte](0)

  final val ReadMode = 'read
  final val WriteMode = 'write

  object Helpers {
    def isCollectionVerb(ctx: RootContext[_]): Boolean = {
      ctx.verb match {
        case verb: MKCOL => true
        case _ => false
      }
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

  logger.debug("resolvedFilePath = %s".format(resolvedFilePath))

  // Apparently these are the API's way to communicate content to API's clients.
  // See for example [[net.vrijheid.clouddrive.pipes.webdavcmds.GETSink]].
  var input : InputStream = _
  var output : OutputStream = _

  // This var is needed because close() has no idea of the mode of the previous open()
  private var _prevOpenMode: Symbol = 'ihavenoidea
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
  private def doReadOpen() {
    _tmpReadFile = File.createTempFile(Integer.toHexString(System.identityHashCode(this)), ".pj")
    logger.debug("Created empty %s for %s".format(_tmpReadFile.getAbsolutePath, resolvedFilePath))
    _close_post_processor = () => {
      _tmpReadFile.delete()
      logger.debug("Deleted %s for %s".format(_tmpReadFile.getAbsolutePath, resolvedFilePath))
    }

    val tmpOutputStream = new FileOutputStream(_tmpReadFile)
    val future = client.getObject(connInfo, container, resolvedFilePath, null, tmpOutputStream)
    try {
      future.get()
      input = new FileInputStream(_tmpReadFile)
    }
    finally {
      tmpOutputStream.close()
    }
  }

  private def doWriteOpen() {
    if(exists(resolvedFilePath)) {
      setOriginal(resolvedFilePath, resolvedFilePath)
      create()
    } else {

    }
  }

  def open(mode: Symbol) = {
    logger.debug("open(%s)".format(mode))
    this._prevOpenMode = mode

    val result = mode match {
      case ReadMode =>
        doReadOpen()
        true

      case WriteMode =>
        doWriteOpen()
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
    logger.debug("close() [_isClosed=%s]".format(_isClosed))
    if(_isClosed) {
      return
    }

    try {
      this._prevOpenMode match {
        case ReadMode =>
          doReadClose()

        case WriteMode =>
          doWriteClose()

        case _ =>
      }
    }
    finally {
      _isClosed = true
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

  def transfer() {
    logger.debug("transfer()")
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
      logger.debug("Create collection %s".format(resolvedFilePath))
      createCollection(resolvedFilePath)
    }
    else {
      logger.debug("Create file %s".format(resolvedFilePath))
      createResource(resolvedFilePath)
    }
    false
  }

  def deleteDirectly(uuid: String) {
    logger.debug("deleteDirectly(%s)".format(uuid))
  }

  def delete() {
    logger.debug("delete()")
  }

  def exists() = {
    val result = exists(resolvedFilePath)
    logger.debug("exists() => %s".format(result))
    result
  }

  def isCollection() = {
    val result = isCollection(resolvedFilePath)
    logger.debug("isCollection() = %s".format(result))
    result
  }

  def getOriginalName() = {
    // FIXME: the full path in Pithos+
    // FIXME: What is the relation of this full path with the resolvedFilePath?
    val original = getOriginal(resolvedFilePath)
    logger.debug("getOriginalName() => %s".format(original))
    original
  }
}
