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
	class COPYSink[T](implicit val ctx: RootContext[T])  extends Sharing[T] with PipeSink with Serializable  {

		private val protocol = (Config("protocol")).trim;

		//HTTP Header out
		var header :Array[Byte] = _
		//We need this later on, here for efficency reasons
		private val slash = ("/" charAt(0))
		private val predicate = { (p : Char) => { p == slash}}


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


		//NOTE: We ignore the Overwrite header (always overwrite)
		//Success = 201 header (created)
		override def >|(data : Array[Byte]) {
			
			//We'll need this when copying
			val storage_type : String = ctx.userConfig("storage")
			//Get HTTP resource, destination
			val resource = ctx.verb.header("resource")
			var destination = java.net.URLDecoder.decode(ctx.verb.header.getOrElse("Destination",""))

			header = destination match {

				//"Something is wrong"
				case "" => {(HTTPServerHelper.httpHeader(404,"text/plain",0)) getBytes}

				//We have a destination
				case _ => {

					//TBD: make this recoverable and use hopefully[T]. Low priority.

					val full_resource = followLink(stripTrailingSlash("/" + ctx.user + resource))
					var full_destination = stripTrailingSlash(destination trim)
					//Clean up the http:// part so we have the resource name
					if (full_destination startsWith(protocol)) {
						full_destination = full_destination substring (protocol length)
						full_destination = (full_destination.indexWhere(predicate,0)) match {

							case -1 => {full_destination}
							case _ => {full_destination substring (full_destination.indexWhere(predicate,0))}
						}
					}
					full_destination = ("/" + ctx.user) + full_destination; 

					//Is it a collection of a resource
					isCollection(full_resource) match {

						//Not yet copying of tag folders from or to....
						case true if((allowed(full_destination)  && !(inTagFolder(full_resource)) && !(inTagFolder(full_destination))  )) => {
							//Get the prefix from the source (its index)
							//Guard it with allowed
							val prefix = (full_resource length) + 1
							//Get the resource list, and reverse so we go from top to bottom
							val resources = (treeAsList(full_resource)) reverse;
							//Loop over the list
							resources foreach {
								(resource) => {
									//Destination = destination_path + (resource - prefix)
									val destination = stripTrailingSlash(full_destination + "/" + (safeSubstring(resource,prefix)))
									debug("Deep copy, destination is: " + destination)
									debug("Deep copy, resource is: " + resource)
									//create destination, copy resource data, delete
									//If it exists, delete
									if (exists(destination)) { 
										debug("destination " + destination + " exists")
										val new_key = ((stripTrailingSlash(resource)).substring(1 + (ctx.user length)))
										val store = Storage.mkStorage(storage_type,new_key)
										store delete;
										deleteTree(destination)
									}
									//Create the new node
									//Copy resource only when it is not a collection
									var new_resource = ""
									if(!isCollection(stripTrailingSlash(resource))) {
										//Get a storage instance for the "source"
										val new_key = ((stripTrailingSlash(resource)).substring(1 + (ctx.user length)))
										val store = Storage.mkStorage(storage_type,new_key)
										debug("store made")
										//Call copy on the Storage layer
										new_resource = store.copy										
									}
									//Create the metadata representation
									createResource(destination)
									//Transfer the metadata
									copyResourceData(resource,destination)
									//Set the original. Note that on a collection it just becomes the empty string because of the initialization above
									setOriginal(destination,new_resource)
									//Add the destination to the tagsets
									addFileToTagSets(ctx.user,destination)
								}
							}
							(HTTPServerHelper.httpHeader(200,"text/plain",0)) getBytes
						}
							
						//It's just a resource
						//Not yet, copying to tag folders
						case false if((allowed(full_destination) && !(inTagFolder(full_destination))  )) => {
							//Generate a "new key" to create a Storage instance for the "source"
							debug("making storage")
							val new_key = (full_resource substring (1 + (ctx.user length)))
							val store = Storage.mkStorage(storage_type,new_key)
							debug("store made")
							//Call copy on the Storage layer
							val new_resource = store.copy
							//If it exists in ZooKeeper, delete it + all children that hold metadata
							//Also delete the actual data
							if (exists(full_destination)) {
								//If we already had data under this key delete it
								val new_key = ((stripTrailingSlash(full_destination)).substring(1 + (ctx.user length)))
								val del_store = Storage.mkStorage(storage_type,new_key)
								del_store.delete()
								//Now delete metadata 
								deleteTree(full_destination)
							}
							//Create the new node
							createResource(full_destination)
							//Transfer the metadata
							copyResourceData(full_resource,full_destination)
							//Add the destination to the tagsets
							addFileToTagSets(ctx.user,full_destination)
							//Set the original to the copy
							setOriginal(full_destination,new_resource)
							//Generate the Header
							(HTTPServerHelper.httpHeader(200,"text/plain",0)) getBytes
						}
						
						case _ => {(HTTPServerHelper.httpHeader(404,"text/plain",0)) getBytes}
					}
				}
			}
		}

		override def <| () : Array[Byte] = {
			ctx.phase = 'allwritten
 			header
		}
	}//Class COPYSink
}//package