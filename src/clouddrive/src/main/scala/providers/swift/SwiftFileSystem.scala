/*
 * Cloud backed storage
 * 
 * This code was developed as part of the activities of the GT-CNC (Grupo
 * de Trabalho Computacao em Nuvem para Ciencia) of RNP (Rede Nacional de
 * Ensino e Pesquisa - www.rnp.br) Brazil. This workgroup is coordinated
 * by Roberto Araujo.
 *
 * Developer: Guilherme Maluf Balzana (guimalufb at gmail.com)
 * Copyright (c) 2013
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * *    Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 * *    Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 * *    Neither the name of vrijheid.net nor the
 *      names of its contributors may be used to endorse or promote products
 *      derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package providers.swift 

import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.httpsupport._
import net.vrijheid.clouddrive.providers.{MetaData}
import net.vrijheid.clouddrive.sharing.{Sharing}
import net.vrijheid.clouddrive.control.{Storage, RootContext}
import net.vrijheid.clouddrive.config.Config

import java.io.{FileInputStream, FileOutputStream, OutputStream, InputStream, File}
import java.lang.Object._

import org.openstack.swift._
import org.openstack.swift.api._
import org.openstack.swift.model._
import org.openstack.keystone._
import org.openstack.keystone.api._
import org.openstack.keystone.model._
import org.openstack.keystone.utils._
import org.openstack._

object SwiftFileSystem {
  object Keys {
    final var AuthURL     = "swift-auth-url"
    final val Username    = "swift-username"
    final val Password    = "swift-password" 
    final val TenantName  = "swift-tenant"
    final val Container   = "swift-container"
  }                   

  final val DefaultContainer  = "clouddrive"

  final var ReadBufferSize    = 1024 * 4 
  final val EmptyByteArray = new Array[Byte](0)

  final val ReadMode  = 'read
  final val WriteMode = 'write
  final val UndefinedMode = 'undefined

  object Helpers {
    def isCollectionVerb(ctx: RootContext[_]): Boolean = {
      ctx.verb match {
        case verb: MKCOL => true
        case _ => false
      }
    }
    def newTmpFile : File = {
      File.createTempFile(Integer.toHexString(System.identityHashCode(this)), ".pj")
    }
  }
                       
}                      

object FSSCounter {
  @volatile private var _counter = 0

  def nextValue = {
    _counter += 1
    _counter
  }
}

// Looks like filepath is the metadata_filepath
class SwiftFileSystem[T](filepath : String)(implicit ctx : RootContext[T]) extends Sharing[T] with Storage {
// key -> filepath
// fullkey -> resolvedFilePath

  import SwiftFileSystem._

  debug("### %s #############################".format(FSSCounter.nextValue))
  debug("SwiftFileSystem: filepath = " + filepath)

  val userID = ctx.user
  private val os_username = Config(Keys.Username) 
  private val os_password = Config(Keys.Password)
  private val os_tenant   = Config(Keys.TenantName)
  private val os_auth_url = Config(Keys.AuthURL)

  // This will get the whole container path(e.g container/dir/subdir) through out the whole file
  //private val os_container= Config(Keys.Container,userID)
  private val os_container= Config(Keys.Container,userID)

  private var keystoneClient : KeystoneClient = new KeystoneClient(os_auth_url)

  //access with unscoped token
  private var accessInfo : Access = keystoneClient.execute(Authenticate.withPasswordCredentials(os_username,os_password))
  //use token in the following requests
  keystoneClient.setToken(accessInfo.getToken.getId)
  //access with previous token and TenantName
  accessInfo = keystoneClient.execute(Authenticate.withToken(accessInfo.getToken.getId).withTenantName(os_tenant))

  //get swift endpoint in keystone and provide previous access Token
  private var swiftClient = new SwiftClient(KeystoneUtils.findEndpointURL(accessInfo.getServiceCatalog, "object-store", null, "public"), accessInfo.getToken.getId)


  debug("ConfigInfo: username=%s, password=%s, tenant=%s, auth_url=%s, container=%s".format(os_username, os_password, os_tenant, os_auth_url, os_container))

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

  val remoteFilePath = stripLeadingSlash(filepath)


  // Set the UUID for storing object on cloud side
  private var fileUUID = (exists(resolvedFilePath)) match {
    case true => { getOriginal(resolvedFilePath) }
    case false => {UUID}
  }

  // Will create container if default doesn't exist 
  swiftClient.execute( new CreateContainer(os_container) )

  debug("resolvedFilePath = %s, remoteFilePath = %s, UUID = %s".format(resolvedFilePath, remoteFilePath,UUID))

  /** TODO something has to be done with directory structure
  if ( remoteFilePath.endsWith("/") ){ //is a directory
     val directory = remoteFilePath.split("/").last

     // Create directory and append it to container path
     // Directory is create in plain text cause getOriginal doesn't treat directories
     swiftClient.execute( new CreateDirectory(os_container,directory)
     
     os_container += "/" + directory
  }
  else { //is a file
      //grep the whole dir/subdir path that was created when the user was going inside the 
  } 


  
  * swiftClient.execute( new CreateDirectory(container
  * swft.execute( new ListObjects("clouddrive", new java.util.HashMap[String, String] ) )
  **/

  // Apparently these are the API's way to communicate content to API's clients.
  // See for example [[net.vrijheid.clouddrive.pipes.webdavcmds.GETSink]].
  var input : InputStream = _
  var output : OutputStream = _

  // This var is needed because close() has no idea of the mode of the previous open()
  private var _prevOpenMode = UndefinedMode
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

  private def openForRead {
    debug("Open for read %s(%s)".format(remoteFilePath,fileUUID))

    input = null
    // Define the object wich will be downloaded
    // TODO treat directory
    val download = swiftClient.execute(new DownloadObject(os_container,fileUUID))

    try {
        input = download.getInputStream
    }
  }

  private def openForWrite {
    create()
    setOriginal(resolvedFilePath, fileUUID)
    debug("setOriginal(%s, %s)".format(resolvedFilePath, fileUUID))
  }

  def open(mode: Symbol) : Boolean = {
    val result = mode match {
      case ReadMode => 
        openForRead
        true
      
      case WriteMode => 
        openForWrite
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
    debug("[_isClosed=%s]".format(_isClosed))
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
        debug("[Unknown mode = %s]".format(mode))
      }
    }
    finally {
      _isClosed = true
      _prevOpenMode = UndefinedMode
    }
  }

  def read(): Array[Byte] = {
    if(input ne null) {
      val buffer = new Array[Byte](ReadBufferSize)

      input.read(buffer) match {
        case -1 =>
          debug("read() => [length=-1]")
          EmptyByteArray

        case n =>
          debug("read() => [length=%s]".format(n))
          buffer.slice(0, n)
      }
    }
    else {
      debug("read() [<no input>] => EmptyByteArray")
      EmptyByteArray
    }
  }

  def write(data: Array[Byte]) {
    val description = if(data eq null) "<null>" else "length=%s".format(data.length.toString)
    debug("write(%s)".format(description))

    if((null ne output) && (null ne data)) {
      output.write(data)
    }
  }

  def copy() = {
    debug("copy()")
    val newUUID = UUID

    // AFAIK there is no copy method in openstack-java-sdk, so this is a ugly workaround. download and upload with different UUID
    val download = swiftClient.execute(new DownloadObject(os_container,fileUUID))

    val upload = new ObjectForUpload
    upload.setContainer(os_container)
    upload.setName(newUUID)
    upload.setInputStream( download.getInputStream ) 

    swiftClient.execute( new UploadObject(upload) )

    newUUID
  }

  def getMetaData() = {
    val result = getMetaData(resolvedFilePath)
    debug("getMetaData() => %s".format(result))
    result
  }

  def transfer() {
    debug("transfer()")
    if(output ne null) {
      output.flush()
      debug("Transfering from %s to remote %s".format(_tmpWriteFile, remoteFilePath))

      val upload = new ObjectForUpload
      upload.setContainer(os_container)
      upload.setName(fileUUID)
      upload.setInputStream( new FileInputStream(_tmpWriteFile) )

      val result = swiftClient.execute( new UploadObject(upload) )

      debug("Transferred, result = %s".format(result))
      debug("result.statusInfo = %s, result.statusText = %d".format(result.getStatusInfo, result.getStatus))
      output = null
      if(result.getStatus != 201) {
        throw new Exception(
          "file = %s, result.statusCode = %s, result.statusText = %d".format(
            remoteFilePath,
            result.getStatusInfo,
            result.getStatus))
      }
    }
  }

  def transferLocalFile(src: File, newuuid: String) {
    debug("transferLocalFile(%s, %s)".format(src.getCanonicalPath, newuuid))
  }


  def setMetaData(metadata: Map[String, String]) {
    setMetaData(resolvedFilePath, metadata)
    debug("setMetaData(%s)".format(metadata))
  }

  def hasChildren() = {
    val result = hasChildren(resolvedFilePath)
    debug("hasChildren() => %s", result)
    result
  }

  // This seems to be used for open('write)
  def create() = {
    if(Helpers.isCollectionVerb(ctx)) {
      debug("Create collection %s (%s)".format(resolvedFilePath, remoteFilePath))
      createCollection(resolvedFilePath)
    }
    else {
      debug("Create file %s (%s)".format(resolvedFilePath, remoteFilePath))
      createResource(resolvedFilePath)
      _tmpWriteFile = Helpers.newTmpFile
      output = new FileOutputStream(_tmpWriteFile)
      debug("Created output=_tmpWriteFile in %s".format(_tmpWriteFile))
      _close_post_processor = () ⇒ {
        _tmpWriteFile.delete()
        debug("Deleted _tmpWriteFile %s for %s".format(_tmpWriteFile.getAbsolutePath, resolvedFilePath))
        _tmpWriteFile = null
      }
    }
    true
  }

  def deleteDirectly(uuid: String) {
    swiftClient.execute(new DeleteObject (os_container,uuid))
  }

  def delete() {
    if(exists) {
      val deleted =  new DeleteObject (os_container,fileUUID)
      val result = swiftClient.execute(deleted)
      debug("Transferred, result = %s".format(result))
      debug("result.statusInfo = %s, result.statusText = %d".format(result.getStatusInfo, result.getStatus))
      output = null
      if(result.getStatus != 204) {
        throw new Exception(
          "file = %s, result.statusCode = %s, result.statusText = %d".format(
            remoteFilePath,
            result.getStatusInfo,
            result.getStatus))
      }
    }
  }

  def exists() = {
    val result = exists(resolvedFilePath)
    debug("exists => %s".format(result))
    result
  }

  def isCollection() = {
    val result = isCollection(resolvedFilePath)
    debug("isCollection => %s".format(result))
    result
  }

  def getOriginalName() = {
    // Get the UUID stored in /clouddrive under the key in Metadata store
    debug("getOriginalName=> %s".format(fileUUID))
    fileUUID
  }
}

