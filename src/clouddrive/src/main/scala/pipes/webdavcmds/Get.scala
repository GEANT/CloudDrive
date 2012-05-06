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
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.providers._
import net.vrijheid.clouddrive.httpsupport._
import net.vrijheid.clouddrive.sharing._
import java.io.{InputStream,Serializable}
import java.net._
import scala.actors._

package net.vrijheid.clouddrive.pipes.webdavcmds {
	
	
	//
	class GETSink[T](implicit val ctx: RootContext[T])  extends Sharing[T] with PipeSink  with Serializable  {
		
		private var remaining: Long = 0
    	private var actualLength: Long = 0
		private var datasource: InputStream = _
		private var header : Array[Byte] = _
		private var resource_exists = false
		private var fullkey : String = _
		private var dir_listing = ""
		
		override def ||() {
			
			val fullpath = "/" + ctx.user + ctx.verb.header("resource")
			
			if(!allowedAccess(fullpath,ctx.user,ctx.verb)) {
				debug("GET: opening store, No Access. Generating 403 response.")
				header = (HTTPServerHelper httpHeader(403,"text/plain","")) getBytes;
				ctx.bypass.write(header)
				ctx.bypass.close()	
				//Necessary to kill the thread
				throw new Exception()
			}		
		}
		
		//On Init, check for existence and setup 404 or data stream
		override def >|(data : Array[Byte]) {
			
			debug("GET sink, entering >|")
			
			resource_exists = ctx.store.exists()
			debug ("resource exists:")
			debug(resource_exists toString)
			
			fullkey = followLink(stripTrailingSlash("/" + ctx.user + ctx.verb.header("resource")))
			
			debug("fullkey")
			debug(fullkey)
			
			resource_exists match {
				//Open storage here etc
				case true => {
					debug("resource exists for GET, >|")
					isCollection(fullkey) match {
						//resource data. prepare for streaming back
						case false => {
							debug("....and is a file")
							debug("with guid = "+getOriginal(fullkey))
							// Open the data stream
							val store = ctx.store
							debug("Opening storage")
							try { store open 'read } catch {
								case e: java.io.FileNotFoundException => {
									debug("GET: opening store, FileNotFound. Generating 404 response.")
									header = (HTTPServerHelper httpHeader(404,"text/plain","")) getBytes;
									ctx.bypass.write(header)
									ctx.bypass.close()
									//TBD: improve this, though it's an effective way on error. Most clients try again anyway.
									throw new Exception()
								}
							}
							datasource = store.getInputStream
							//Get content size, type
							actualLength = (getMetaData(fullkey,"getcontentlength")) toLong;
							val contenttype = getMetaData(fullkey,"getcontenttype");
              				//Get actual data length (after encryption)
              				remaining = getPutLength(fullkey)
              				debug("remaining data length = " + remaining.toString)
							//Pre-generate header
							header = (HTTPServerHelper httpHeader (200,contenttype,actualLength)) getBytes
						}
						//Collection -> directory listing for web browsers
						case true => {
							debug("and is also a collection, generating html listing")
							val children = getChildren(fullkey)
							var html = "<HTML><HEAD></HEAD><BODY><UL>"
							var resource = stripTrailingSlash(ctx.verb.header("resource"))
							//Loop to add children to HTML here in a fashionable way...
							children foreach { (child) => {
									isCollection(followLink(fullkey + "/" + child)) match {
										//Link to directories versus files
										case true => {
											html += "<LI>DIR:&nbsp;" + "<a href=\"" + resource + "/" + child  + "\">" +  URLDecoder.decode(child) + "</a>"									
										}
										
										//Resources (files)
										case false => {
											html += "<LI>FILE:&nbsp;" + "<a href=\"" + resource + "/" + child  + "\">" +  URLDecoder.decode(child) + "</a>"									
										}
									}
								}
							}
							dir_listing += "</UL></BODY></HTML>"
							dir_listing = html
							header = (HTTPServerHelper httpHeader (200,"text/html",(dir_listing length))) getBytes
						}
					}
				}
				//404
				case false => { 
					debug("resource does not exist for GET, >|")
					header = (HTTPServerHelper httpHeader(404,"text/plain","")) getBytes
				}
			}
			
		}
		
		override def <| (): Array[Byte] = {
			
			debug("GET sink, entering <|")
			debug("resource_exists")
			debug((resource_exists)toString)
						
			resource_exists match {
				//Stream contents back from  store 
				case true => {
					(dir_listing isEmpty) match {
						case true => {
							//Check to see if we need to push the HTTP header, or can push data
							debug("Header length")
							debug((header length)toString)
							(header length) match {
								case 0 => {
                  					debug("writing data")
									val store = ctx.store
									val data = store read;
                  					debug("Length of data read from store = " + (data length).toString)
									remaining -= (data length)
									debug("remaining")
									debug((remaining)toString)
									//Check if we're done
									if  (0 >= remaining ) { 
										//store close;
										ctx.phase = 'allwritten
									}
									data
								}
					
								case _ => { 
									debug("Resource exists, writing header and optionally dir_listing")
									val data = header
                  					debug("header = " + new String(data))
									//This flips us to the case 0 state above
									header = Array[Byte]()
                  					//Bypass the pipe because the header needs to be written unencrypted
									ctx.bypass.write(data)
                  					debug("Header written using bypass")
                  					header //Empty Array now
								}
							}
						}
						//Nope, we return a HTML dir listing and are done immediately
						case false => {
							ctx.phase = 'allwritten
              				//Bypass the pipe because the header needs to be written unencrypted
							ctx.bypass.write(header ++ (dir_listing getBytes))
              				Array[Byte]()
						}
					}
				}
				//Return the 404
				case false => {
					ctx.phase = 'allwritten
					debug("State false, writing 404")
          			//Bypass the pipe because the header needs to be written unencrypted
					ctx.bypass.write(header)
          			Array[Byte]()
				}
			}
			
		}
		
		override def |<| (): Array[Byte] = {
			Array[Byte]()
		}
		
	}
	
	
}