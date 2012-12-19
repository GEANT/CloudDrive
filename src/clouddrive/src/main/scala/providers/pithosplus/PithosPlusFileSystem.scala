package providers.pithosplus

import net.vrijheid.clouddrive.control.{Storage, RootContext}
import net.vrijheid.clouddrive.sharing.Sharing
import java.io.{OutputStream, InputStream, File}

class PithosPlusFileSystem[T](key : String)(implicit ctx : RootContext[T]) extends Sharing[T] with Storage {
  println("PithosPlusFileSystem: key = " + key)
  final val userToken = ctx.userConfig.get("pithos-user-token") // ???
  final val baseURL   = ctx.userConfig.get("pithos-base-url") // ???

  var input : InputStream = _
  var output : OutputStream = _

  def open(mode: Symbol) = false

  def close() {}

  def read() = null

  def write(data: Array[Byte]) {}

  def copy() = null

  def getMetaData() = null

  def transfer() {}

  def transferLocalFile(src: File, newuuid: String) {}

  def setMetaData(metadata: Map[String, String]) {}

  def hasChildren() = false

  def create() = false

  def deleteDirectly(uuid: String) {}

  def delete() {}

  def exists() = false

  def isCollection() = false

  def getOriginalName() = null
}
