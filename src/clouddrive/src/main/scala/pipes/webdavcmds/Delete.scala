/*
 * Cloud backed storage
 * 
 * Copyright (c) 2010-2012, vrijheid.net
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
import net.vrijheid.clouddrive.pipes._
import net.vrijheid.clouddrive.sharing._
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.providers.{Locker}
import net.vrijheid.clouddrive.httpsupport.{HTTPServerHelper} 
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes.webdavcmds {
	
	//
	class DELETESink[T](implicit val ctx: RootContext[T])  extends Sharing[T] with PipeSink with Serializable   {
		
		private var header = ""

		override def ||() {
			
			val fullpath = stripTrailingSlash( "/" + ctx.user  + ctx.verb.header("resource"))
			
			if(!allowedAccess(fullpath,ctx.user,ctx.verb)) {
				debug("GET: opening store, No Access. Generating 403 response.")
				val header = (HTTPServerHelper httpHeader(403,"text/plain","")) getBytes;
				ctx.bypass.write(header)
				ctx.bypass.close()	
				//Necessary to kill the thread
				throw new Exception()
			}		
		}
		
		override def >|(data : Array[Byte]) {

			val storage_type = ctx.userConfig("storage")
			val path = stripTrailingSlash("/" + ctx.user + ctx.verb.header("resource"))
			
			//TODO-TAG
			//Switch on collection or resource
			//This elaborate condition initially prohibits DELETES from links (search results) in tag folders as well
			//Later, we'll switch and then just delete the tags on the file and remove the link
			//Also, prevent deleting the tag, pyblic and common root folder
			if(allowed(path) && !(inTagFolder(path)) && !(isTagFolder(path)) && !(isPublicFolder(path)) && !(isCommonFolder(path) )   ) {
				ctx.store.isCollection() match {
				
					case true => {
						debug("It is a collection in DELETE, >|")
						//This will delete a specific resource or empty collection

						if (ctx.store hasChildren) {
							debug("It is an empty collection in DELETE, >|")
							//Delete the resource
							ctx.store.delete();
							//Delete from ZooKeeper
							deleteTree(path);						
						} else {
							debug("It is an non-empty collection in DELETE, >|")
							//We have children, so let's process recursively
							val deleter = { 
								(key : String) => {
									debug("In deleter, key = " + key)
									debug("making storage")
									val new_key = (key substring (1 + (ctx.user length)))
									val store = Storage.mkStorage(storage_type,new_key)
									debug("store made")
									store.delete();
									deleteTree(key)
								}
							}
					
							//We will walk the tree bottom up, so if we fail, data simply is still there
							processTree(path,deleter)
						}//else
						header = HTTPServerHelper.httpHeader(200,"text/plain",0)
					}//case true
				
					case false => {
						debug("It is NOT a collection in DELETE, >|")
						//Delete the resource from the store
						ctx.store.delete();
						//Delete from ZooKeeper
						delete(path);
						header = HTTPServerHelper.httpHeader(200,"text/plain",0)
					}
				}
			}
			//Not allowed due to locks
			else {header = HTTPServerHelper.httpHeader(423,"text/plain",0)}
		}
		
		override def <| (): Array[Byte] = {
			//CODE_CC if it was a delete from a shared folder, call the removeFolderFromSharedFolder (or File...From...)

			debug("DELETE sink, entering <|")
			debug("returning header")

			ctx.phase = 'allwritten
			debug("State allwritten, writing header")
			header getBytes

		}
	}

}
