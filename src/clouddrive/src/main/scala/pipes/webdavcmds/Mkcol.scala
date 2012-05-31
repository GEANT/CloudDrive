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
import net.vrijheid.clouddrive.httpsupport._
import net.vrijheid.clouddrive.providers._
import java.util.Date
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes.webdavcmds {
	
	//
	class MKCOLSink[T](implicit val ctx: RootContext[T])  extends Sharing[T] with PipeSink with Serializable  {
		
		/* Get the header, 
		check if resource, 
		check if resource exists (409)
		create resource in ZooKeeper
		Create and return header (201)
		*/

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

		
		private val fullpath = java.net.URLDecoder.decode(followLink("/" + ctx.user + stripTrailingSlash(ctx.verb.header("resource"))))
		
		private var header: Array[Byte] = _
		
		override def >|(data : Array[Byte]) {
			
			debug("In MKCOL, >|")
			var resource = stripTrailingSlash(ctx.verb.header("resource"))
			debug("resource = " + resource)
			debug("fullpath = " + fullpath)
			//TODO-TAG
			//This prohibits us from making folders in tag folders
			//Later, we'll allow this and make intelligent searches based on this functionality
			//if (exists(fullpath) || isTagFolder(fullpath) || inTagFolder(pathFromFullPath(fullpath))) {
			if (exists(fullpath) && isTagFolder(fullpath)) {
					debug("Collection does exist or is a Tagfolder")
					header = (HTTPServerHelper.httpHeader(409,"text/plain",0)) getBytes;
			} 
			//This creates a new Tagfolder if it does not exists and is a Tag folder
			else if(!(exists(fullpath)) && inTagFolder(fullpath)) {
				debug("In MKCOL, creating new tag folder and updating its contents")
				val foldername = fileNameFromPath(fullpath)
				val tags: List[String] = foldername.split(" ").map(_.trim).toList;
				createTagFolder(fullpath,tags,Map())
				header = (HTTPServerHelper.httpHeader(201,"text/plain",0)) getBytes;
			}
			else {
				//This creates a new normal folder
				if (ctx.store.create) {
					debug("Created collection")
					header = (HTTPServerHelper.httpHeader(201,"text/plain",0)) getBytes;
					val now = isoDateNow()
					ctx.store.setMetaData(Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> now, davEncode("getlastmodified") -> now))
				} else {
					//"Something went wrong"
					debug("Failed to create collection")
					header = (HTTPServerHelper.httpHeader(409,"text/plain",0)) getBytes;
				}
			}
		}
		
		override def <| (): Array[Byte] = {	
			debug("MKCOL Command Sink <|: " + new String(header))
			ctx.phase = 'allwritten
			header 
		}	
		
	}
	
}