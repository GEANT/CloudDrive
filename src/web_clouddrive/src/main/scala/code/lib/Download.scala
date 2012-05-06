/*
 * Cloud drive website
 * 
 * Copyright (c) 2010-2012, vrijheid.net

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
package net.vrijheid.clouddrive.website.code {
	package lib {
		package files {
			import net.vrijheid.clouddrive.website.code.lib.login._
			import net.vrijheid.clouddrive.control._
			import net.vrijheid.clouddrive.utils._
			import net.vrijheid.clouddrive.pipes._
			import net.vrijheid.clouddrive.providers._
			import net.vrijheid.clouddrive.config._
			import net.vrijheid.clouddrive.httpsupport._
	        import net.liftweb.common.{ Box, Full, Empty }
		    import net.liftweb.http._
		    import net.liftweb.util.Helpers._		
		    import java.io._
						
			class LiftDownloadSink[T](implicit val ctx: RootContext[T])  extends MetaData[T] with PipeSink with Recoverable {
				
				private var remaining: Long = 0
		    	private var actualLength: Long = 0
				private var datasource: InputStream = _
				private var contenttype: String = _
				val store = ctx.store
				val fullkey = stripTrailingSlash("/" + ctx.user + ctx.verb.header("resource"))


				def correctfailure(c: Context) {
					val ctx = c.asInstanceOf[LiftContext]
					//On failure, delete the nmetadata key, the data does not exist....
					deleteTree(fullkey)
					S.warning(S ? "The requested file was no more found.")
				}				
				
				
				//make sure we move from ctx.phase 'outgoing to 'allwritten , see pipes.webdavcmd.GET 
				override def||(){
					recoverable(
						{
						store open 'read
						datasource = store.getInputStream
	      				//Get actual data length (after encryption)
	      				remaining = getPutLength(fullkey)
						debug("|| init passed")
						},correctfailure _
					)
				}
				
				override def <|(): Array[Byte] = {
					val data = store read;
					debug("Read "+data.length+"bytes from store")
					remaining -= (data length)
					debug("remaining: "+remaining)
					//Check if we're done
					if  (0 >= remaining ) { 
						debug("In <|, all read (0 >= remaining)")
						ctx.phase = 'allwritten
					}
					data
				}
				
			}
						
			class LiftDownloadManager extends PipeSource with Recoverable {
				
				//Init the user
				val user = Login.getUserEmail.openOr("")
				//Create a "fake" HTTP verb to work with drive context
				//First guard against weirness with a trailing dot from Lift
				val res = S.param("resource").openOr("/")
				val resource = res endsWith "." match {
					case true => { res.slice(0,-1 + res.length)}
					case false => res
				}
				val headers = Map("resource" -> resource);		    	
				lazy val verb = (headers contains "resource") match {
					case true => new WEBSITE(headers,'get)
					//Safeguard
					case false => new WEBSITE(headers ++ Map("resource" -> "/"),'get)
				}
				
				debug("In LiftDownloadManager, resource ="+headers("resource"))
				//Create the drive context and set some initials
				implicit lazy val ctx = new LiftContext(verb)
				ctx.user = user
				ctx.userConfig = Config.userConfig(ctx)
				ctx.storageclient = VMTalk getStorageClient;
				ctx.store = Storage.mkStorage(
					ctx.userConfig("storage"),
					(ctx.verb.header("resource"))
				)
				//Create a metatdata store, if needed
				lazy val metadata = new MetaData()
				//The full metadata key to our file
				lazy val fullkey = metadata.followLink(stripTrailingSlash("/" + user + verb.header("resource")))
				//This will be used in more then one place, hence on instance level
				val contentlength = metadata.getMetaData(fullkey,"getcontentlength");
				
				//Init the pipe, we act as source ourselves
				lazy val pipe =  new Pipe(this, new LiftDownloadSink()(ctx))
				pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
				pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
				pipe ++ Pipe.mkQuota();
				pipe ++ Pipe.mkDecompressor(ctx.userConfig("compression","gzip"))
				pipe ++ Pipe.mkDecryption(ctx.userConfig("decryption","aes"))
				
				//This will hold the temp result as a buffer to bridge between PipeSource and Streaming response
				var resultBuf: Array[Byte] = Array[Byte]()
				var wrapped_up = false
				
			
				override def <|(data: Array[Byte]) {
					resultBuf = resultBuf ++ data
				}
				
				override def |<|(data: Array[Byte]) {
					resultBuf = resultBuf ++ data
				}
				
				def initPipe(){
					pipe ||;
					ctx.phase = 'outgoing					
				}
				
				def wrapUpDownload() {
					debug("closing store, wrapping up streaming response")
					ctx.store.close()
				}
				
				def getHeaders() = {
					("Content-type" -> "application/binary") :: ("Content-length" -> contentlength) :: Nil
				}
				
				def validFile_?() = {
					((metadata.exists(fullkey)) & (! metadata.isCollection(fullkey)))
				}
				
				def getContentLength = {
					debug("getContentLength ="+contentlength)
					contentlength toLong;
				}
				
				
				
				//This method
				def read(buffer: Array[Byte]): Int = {
					
					debug("inside read")
					//Depending on the phase, execute action
					ctx.phase match {
						
					    case 'outgoing => { 
							debug("state is outgoing")
							pipe.<|
							
							var length = resultBuf.length
							//Due to (de-)compression block size, we might have more data then the incoming buffer can handle
							(resultBuf.length > buffer.length) match {
								
								case true => {
									Array.copy(resultBuf,0,buffer,0,buffer.length)
									resultBuf = resultBuf.slice(buffer.length,resultBuf.length)
									length = buffer.length
									
								}
								
								case false => {
									Array.copy(resultBuf,0,buffer,0,resultBuf.length)
									resultBuf = Array[Byte]()	
								}
								
							}
							debug("length bytes written="+length)
							length;
							
						}
						case 'allwritten => {
							debug("state is done")
							debug("buffer length="+buffer.length)
							if(! wrapped_up) {
								debug("write wrapped up, executing |<| once")
								pipe.|<|
								wrapped_up = true
							}
							var length = resultBuf.length
							debug("resultBuf length ="+resultBuf.length)
							//Due to (de-)compression block size, we might have more data then the incoming buffer can handle
							(resultBuf.length > buffer.length) match {
								
								case true => {
									debug("****Sending partial result from LiftDownloadManager read, 'allwritten")
									Array.copy(resultBuf,0,buffer,0,buffer.length)
									resultBuf = resultBuf.slice(buffer.length,resultBuf.length)
									length = buffer.length
								}
								
								case false => {
									debug("****Sending final result from LiftDownloadManager read, 'allwritten")
									Array.copy(resultBuf,0,buffer,0,resultBuf.length)
									resultBuf = Array[Byte]()
									ctx.phase = 'done	
								}
							}							
							debug("***'allwritten, length bytes written="+length)
							length;
						}
						case 'done => -1
						case _ => -1
					}
				}
			}
		

			class LiftPublicDownloadManager extends PipeSource with Treatise {
				
				//Init the user
				val user = S.param("user").openOr("")
				//TBD ERROR
				//if user = "" create redirect to error page
				
				//Create a "fake" HTTP verb to work with drive context
				//First guard against weirness with a trailing dot from Lift
				val res = S.param("resource").openOr("/")
				val public_root_folder = "/" + stripLeadingSlash(Config("public_folder_root"))
				val resource = res endsWith "." match {
					case true => { (S ?  public_root_folder) + res.slice(0,-1 + res.length)}
					case false => (S ? public_root_folder) + res
				}
				
				val headers = Map("resource" -> resource);		    	
				lazy val verb = (headers contains "resource") match {
					case true => new WEBSITE(headers,'get)
					//Safeguard
					case false => new WEBSITE(headers ++ Map("resource" -> "/"),'get)
				}
				
				debug("In LiftPublicDownloadManager, resource ="+headers("resource"))
				//Create the drive context and set some initials
				implicit lazy val ctx = new LiftContext(verb)
				ctx.user = user
				ctx.userConfig = Config.userConfig(ctx)
				ctx.storageclient = VMTalk getStorageClient;
				ctx.store = Storage.mkStorage(
					//TBD change COnfig  to ctx.userConfig
					ctx.userConfig("storage"),
					//The /Public is needed for the valid metadata path
					(ctx.verb.header("resource"))
				)
				//Create a metatdata store, if needed
				lazy val metadata = new MetaData()
				//The full metadata key to our file
				lazy val fullkey = stripTrailingSlash("/" + user + verb.header("resource"))
				
				//CODE_CC: CHECK FOR FOLDER OR FILE.
				//Better error reporting 
				//In case of folder a listing actually
				if((metadata isCollection(fullkey))) { throw new Exception }
				
				//This will be used in more then one place, hence on instance level
				val contentlength = metadata.getMetaData(fullkey,"getcontentlength");
				
				//Init the pipe, we act as source ourselves
				lazy val pipe =  new Pipe(this, new LiftDownloadSink()(ctx))
				pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
				pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
				pipe ++ Pipe.mkQuota();
				pipe ++ Pipe.mkDecompressor(ctx.userConfig("compression","gzip"))
				pipe ++ Pipe.mkDecryption(ctx.userConfig("decryption","aes"))
				
				//This will hold the temp result as a buffer to bridge between PipeSource and Streaming response
				var resultBuf: Array[Byte] = Array[Byte]()
				var wrapped_up = false
				
			
				override def <|(data: Array[Byte]) {
					resultBuf = resultBuf ++ data
				}
				
				override def |<|(data: Array[Byte]) {
					resultBuf = resultBuf ++ data
				}
				
				def initPipe(){
					pipe ||;
					ctx.phase = 'outgoing					
				}
				
				def wrapUpDownload() {
					debug("closing store, wrapping up streaming response")
					ctx.store.close()
				}
				
				def getHeaders() = {
					("Content-type" -> "application/binary") :: ("Content-length" -> contentlength) :: Nil
				}
				
				def validFile_?() = {
					((metadata.exists(fullkey)) & (! metadata.isCollection(fullkey)))
				}
				
				def getContentLength = {
					debug("getContentLength ="+contentlength)
					contentlength toLong;
				}
				
				//This method
				def read(buffer: Array[Byte]): Int = {
					
					debug("inside read")
					//Depending on the phase, execute action
					ctx.phase match {
						
					    case 'outgoing => { 
							debug("state is outgoing")
							pipe.<|
							var length = resultBuf.length
							//Due to (de-)compression block size, we might have more data then the incoming buffer can handle
							(resultBuf.length > buffer.length) match {
								
								case true => {
									Array.copy(resultBuf,0,buffer,0,buffer.length)
									resultBuf = resultBuf.slice(buffer.length,resultBuf.length)
									length = buffer.length
									
								}
								
								case false => {
									Array.copy(resultBuf,0,buffer,0,resultBuf.length)
									resultBuf = Array[Byte]()	
								}
								
							}
							debug("length bytes written="+length)
							length;
							
						}
						case 'allwritten => {
							debug("state is done")
							debug("buffer length="+buffer.length)
							if(! wrapped_up) {
								debug("write wrapped up, executing |<| once")
								pipe.|<|
								wrapped_up = true
							}
							var length = resultBuf.length
							debug("resultBuf length ="+resultBuf.length)
							//Due to (de-)compression block size, we might have more data then the incoming buffer can handle
							(resultBuf.length > buffer.length) match {
								
								case true => {
									debug("****Sending partial result from LiftDownloadManager read, 'allwritten")
									Array.copy(resultBuf,0,buffer,0,buffer.length)
									resultBuf = resultBuf.slice(buffer.length,resultBuf.length)
									length = buffer.length
								}
								
								case false => {
									debug("****Sending final result from LiftDownloadManager read, 'allwritten")
									Array.copy(resultBuf,0,buffer,0,resultBuf.length)
									resultBuf = Array[Byte]()
									ctx.phase = 'done	
								}
							}							
							debug("***'allwritten, length bytes written="+length)
							length;
						}
						case 'done => -1
						case _ => -1
					}
				}
			}
		
			object LiftDownloadManager {
				
				def serveDownload: LiftRules.DispatchPF = { 
				  case Req("download" :: Nil, _, GetRequest) => () => {
					val dlm = new LiftDownloadManager
					dlm initPipe;
					
				    dlm.validFile_? match {
				      case true => Full(StreamingResponse(dlm,dlm.wrapUpDownload _,dlm.getContentLength,dlm.getHeaders,Nil,200))
				      case _ => Empty
				    }
				  }
				}
				
				def publicDownload: LiftRules.DispatchPF = { 
				  case Req("public" :: Nil, _, GetRequest) => () => {
					val dlm = new LiftPublicDownloadManager
					dlm initPipe;
					
				    dlm.validFile_? match {
				      case true => Full(StreamingResponse(dlm,dlm.wrapUpDownload _,dlm.getContentLength,dlm.getHeaders,Nil,200))
				      case _ => Empty
				    }
				  }
				}
				
			}
		}
	}
}