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
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.providers.{Locker}
import net.vrijheid.clouddrive.httpsupport.{HTTPServerHelper}
import net.vrijheid.clouddrive.config._
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes.webdavcmds {
	
	//CODE_CC: make move to a shared file like COPY?
	class MOVESink[T](implicit val ctx: RootContext[T])  extends Sharing[T] with PipeSink with Serializable  {
		
		var header :Array[Byte] = _
		//We need this later on, here for efficency reasons
		private val slash = ("/" charAt(0))
		private val predicate = { (p : Char) => { p == slash}}
		private val protocol = (Config("protocol")).trim;
		
		override def ||() {
			
			val fullpath = stripTrailingSlash("/" + ctx.user + ctx.verb.header("resource"))
			
			if(!allowedAccess(fullpath,ctx.user,ctx.verb)) {
				debug("Move: opening store, No Access. Generating 403 response.")
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
			val resource = ctx.verb.header("resource")
			//Make sure we URL decode the destination
			val destination = java.net.URLDecoder.decode(ctx.verb.header.getOrElse("Destination",""))
			
			header = destination match {
				
				//"Something is wrong
				case "" => {(HTTPServerHelper.httpHeader(404,"text/plain",0)) getBytes}
				
				//We have a destination
				case _ => {
					
					//TBD: make this recoverable with hopefully[T] some day, though most clients don't care so far.
					
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
					full_destination = followLink(("/" + ctx.user) + full_destination)
					debug("full_destination = " + full_destination)

					isCollection(full_resource) match {
						
						//Moving of tag folders, needed for Finder etc. make sure we only move on the tagfolder_root top-level
						case true if((allowed(full_destination)  && (inTagFolder(full_resource)) && (inTagFolder(full_destination))  )) => {
							debug("In MOVE, case where we are going to mve one tag folder to another")
							//ONLY allow the move at tag_folder_root level
							if (isTagFolder(pathFromFullPath(full_destination)) && isTagFolder(pathFromFullPath(full_resource))) {
								debug("MOVE at tag folder root level")
								//First, delete the old tag folder
								debug("Deleting tag folder in MOVE")
								val deleter = { 
									(key : String) => {
										debug("In deleter, key = " + key)
										val new_key = (key substring (1 + (ctx.user length)))
										deleteTree(key)
									}
								}
								processTree(full_resource,deleter)
								//Now, create the new one
								val foldername = fileNameFromPath(full_destination)
								val tags: List[String] = foldername.split(" ").map(_.trim).toList;
								createTagFolder(full_destination,tags,Map())
								(HTTPServerHelper.httpHeader(200,"text/plain",0)) getBytes
							}
							else  {(HTTPServerHelper.httpHeader(404,"text/plain",0)) getBytes}
						}
						
						//Generic move of non tag folders.
						case true if((allowed(full_destination)  && !(inTagFolder(full_resource)) && !(inTagFolder(full_destination))  )) => {
							
							debug("full_destination = " + full_destination)
							if(! exists(full_destination)) {
								createCollection(full_destination)
								copyResourceData(full_resource,full_destination)
							}
							
							debug("MOVE, isCollection")
							//Get the prefix from the source (its index)
							//val prefix = findIndexOfReverse(full_resource,slash)
							val prefix = (full_resource length) + 1
							//Get the resource list, and reverse so we go from top to bottom
							val resources = ((treeAsList(full_resource)) reverse).drop(1)
							debug("resources as list = " + resources)
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
										deleteTree(destination)
									}
									//Create the metadata representation
									isCollection(resource) match {
										case true => {
											createCollection(destination)
											copyResourceData(resource,destination)
										}
										
										case false => {
											//Create the resource
											createResource(destination)
											copyResourceData(resource,destination)
											delete(resource)
											deleteFileFromTagSets(ctx.user,resource)
										}
									}
									debug("***children of destination = "+getChildren(destination))
									//Transfer the metadata
									if(!(isCollection(destination))) {
										addFileToTagSets(ctx.user,destination)
										debug("Added new resource in collection move to tagset")
									}
									debug("***After resource copy, children of destination = "+getChildren(destination))
								}
							}
							//Finally, if all succeeds, delete the source
							deleteTree(full_resource)
							(HTTPServerHelper.httpHeader(200,"text/plain",0)) getBytes
						}
						
						//Guard is to prevent moving tags items around, for now
						case false if((allowed(full_destination) && !(inTagFolder(full_destination))  )) => {
							debug("MOVE, is resource")
							debug("full_destination = " + full_destination)
							//If it exists, delete it + all children that hold metadata
							if (exists(full_destination)) {delete(full_destination)}
							//Create the new node
							if(! exists(full_destination)) {
								createResource(full_destination)
								copyResourceData(full_resource,full_destination)
							}
							//Transfer the metadata
							copyResourceData(full_resource,full_destination)
							//Delete the old one
							//Remove old source from the tagsets
							deleteFileFromTagSets(ctx.user,full_resource)
							debug("Deleted old file from tagsets")
							//Add the destination to the tagsets
							addFileToTagSets(ctx.user,full_destination)
							debug("Added new file from tagsets")
							delete(full_resource)
							(HTTPServerHelper.httpHeader(200,"text/plain",0)) getBytes
						}
						
						case _ => {(HTTPServerHelper.httpHeader(404,"text/plain",0)) getBytes}
					}
				}
			}
		}
		
		override def <| (): Array[Byte] = {
			ctx.phase = 'allwritten
 			header
		}
		
	}
	
}