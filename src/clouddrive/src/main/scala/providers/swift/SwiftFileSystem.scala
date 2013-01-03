package providers.swift

import net.vrijheid.clouddrive.control.{Storage, RootContext}
import net.vrijheid.clouddrive.sharing.Sharing
import java.io.{OutputStream, InputStream, File}

class SwiftFileSystem[T](key : String)(implicit ctx : RootContext[T]) extends Sharing[T] with Storage {
  println("SwiftFileSystem: key = " + key)
  final val username = ctx.userConfig.get("swift-username") // ???
  final val password = ctx.userConfig.get("swift-password") // ???
  final val tenant   = ctx.userConfig.get("swift-tenant") // ???
  final val baseURL  = ctx.userConfig.get("swift-auth-url") // ???

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
