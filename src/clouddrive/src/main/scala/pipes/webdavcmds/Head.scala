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
import net.vrijheid.clouddrive.providers.{MetaData}
import net.vrijheid.clouddrive.httpsupport.{HTTPServerHelper}
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes.webdavcmds {
	
	//
	class HEADSink[T](implicit val ctx: RootContext[T])  extends Sharing[T] with PipeSink  with Serializable {
		
		private var header : Array[Byte] = _

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

		//On Init, check for existence and setup 404 or data stream
		override def >|(data : Array[Byte]) {

			debug("HEAD sink, entering >|")

			val resource_exists = ctx.store.exists()
			debug ("resource exists:")
			debug(resource_exists toString)

			val fullkey = followLink("/" + ctx.user + ctx.verb.header("resource"))

			debug("fullkey")
			debug(fullkey)

			resource_exists match {
				//Open storage here etc
				case true => {

					//Get content size, type
					val remaining = getMetaData(fullkey).getOrElse("getcontentlength","0").toLong
					val contenttype = getMetaData(fullkey).getOrElse("getcontenttype","application/binary");
					//Pre-generate header
					header = (HTTPServerHelper httpHeader (200,contenttype,remaining)) getBytes
				}
				//404
				case false => { 
					header = (HTTPServerHelper httpHeader(404,"text/plain","")) getBytes
				}
			}

		}

		override def <| (): Array[Byte] = {

			debug("HEAD sink, entering <|")
			debug("returning header")

			ctx.phase = 'allwritten
			debug("State allwritten, writing header")
			header

		}

	}	
		
}