package providers.pithosplus

import net.vrijheid.clouddrive.control.{Storage, RootContext}
import net.vrijheid.clouddrive.sharing.Sharing
import java.io.{FileOutputStream, OutputStream, InputStream, File}
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

  final val ReadMode = 'read
  final val WriteMode = 'write
}

class PithosPlusFileSystem[T](filepath : String)(implicit ctx : RootContext[T]) extends Sharing[T] with Storage {
  import PithosPlusFileSystem._

  private[this] val logger = LoggerFactory.getLogger(this.getClass)

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
  private var _tmp_outFile_for_read: File = _
  private var _tmp_outStream_for_read: OutputStream = _

  private def doReadOpen() {
    _tmp_outFile_for_read = File.createTempFile(Integer.toHexString(System.identityHashCode(this)), ".pj")
    _tmp_outStream_for_read = new FileOutputStream(_tmp_outFile_for_read)
    val future = client.getObject(connInfo, container, resolvedFilePath, null, _tmp_outStream_for_read)
    future.get()
  }

  private def doWriteOpen() {

  }

  def open(mode: Symbol) = {
    logger.debug("==> open(%s)".format(mode))
    this._prevOpenMode = mode

    mode match {
      case ReadMode =>
        doReadOpen()
        true

      case WriteMode =>
        doWriteOpen()
        true

      case _ =>
        false
    }
  }

  private def doReadClose() {
  }

  private def doWriteClose() {
  }

  def close() {
    logger.debug("close()")
    this._prevOpenMode match {
      case ReadMode =>
        doReadClose()

      case WriteMode =>
        doWriteClose()

      case _ =>
    }
  }

  def read() = {
    logger.debug("read()")
    null
  }

  def write(data: Array[Byte]) {
    logger.debug("write()")
  }

  def copy() = {
    logger.debug("copy()")
    null
  }

  def getMetaData() = {
    logger.debug("getMetaData()")
    null
  }

  def transfer() {
    logger.debug("transfer()")
  }

  def transferLocalFile(src: File, newuuid: String) {
    logger.debug("transferLocalFile(%s, %s)".format(src.getCanonicalPath, newuuid))
  }

  def setMetaData(metadata: Map[String, String]) {
    logger.debug("setMetaData(%s)".format(metadata))
  }

  def hasChildren() = {
    logger.debug("hasChildren()")
    false
  }

  def create() = {
    logger.debug("create()")
    false
  }

  def deleteDirectly(uuid: String) {
    logger.debug("deleteDirectly(%s)".format(uuid))
  }

  def delete() {
    logger.debug("delete()")
  }

  def exists() = {
    logger.debug("exists()")
    false
  }

  def isCollection() = {
    logger.debug("isCollection()")
    false
  }

  def getOriginalName() = {
    logger.debug("getOriginalName()")
    null
  }
}
