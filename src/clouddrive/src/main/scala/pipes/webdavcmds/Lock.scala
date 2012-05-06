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
import scala.xml._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.providers._
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes.webdavcmds {
	
	//
	class LOCKSink[T](implicit val ctx: RootContext[T])  extends Sharing[T] with PipeSink with Serializable  {
	
		private var header = ""
		private var response = ""
		private var xmlbody = Array[Byte]()
		private val lock2User = new LockToUser
		
		
		override def ||()  {
			
			val fullpath = stripTrailingSlash( "/" + ctx.user  + ctx.verb.header("resource"))
			
			if(!allowedAccess(fullpath,ctx.user,ctx.verb)) {
				debug("GET: opening store, No Access. Generating 403 response.")
				val header = (HTTPServerHelper httpHeader(403,"text/plain","")) getBytes;
				ctx.bypass.write(header)
				ctx.bypass.close()	
				//Necessary to kill the thread
				throw new Exception()
			}			
			
			ctx.store.open('lock) 
		}
		
		override def >|(data : Array[Byte]) {
			xmlbody = xmlbody ++ data
			debug("In LOCK, xmlbody = " + new String(xmlbody))
		}
		
		override def |>|(data: Array[Byte]) = {
			val fullkey = followLink(stripTrailingSlash("/" + ctx.user + ctx.verb.header("resource")))
			//First, see if there is a body
			(xmlbody length) match {
			
				//Nope -> refresh lock. Checkk the If header for the lock and resource (eventually...)
				//Get the data
				//Generate response
				case 0 => {
					//Do we have a matching If: header in the HTTP request
					allowed(fullkey) match {
						//No, error.
						case false => {header = HTTPServerHelper httpHeader(404,"text/plain","")}
						//Yes: Refresh and generate response
						case true => {
							//Note this is fairly expensive
							var lock_id : String = (getLock(fullkey)).get
							val lockdata = getLockData(fullkey,lock_id)
							lock_id = refreshLock(fullkey,lockdata)
							//This is for Win7 Explorer
							//We need to be able to map a lock back to user in case of a request WITH lock but without authentication
							lock2User addLock(lock_id,ctx.user)
							//generate response
							response += """<?xml version="1.0" encoding="utf-8" ?> <D:prop xmlns:D="DAV:"><D:lockdiscovery><D:activelock> """
							response += lockdata
							response += "<D:timeout>Second-600000</D:timeout>"
							response += "<D:locktoken><D:href>opaquelocktoken:" + lock_id + "</D:href></D:locktoken>"
							response += "</D:activelock></D:lockdiscovery></D:prop>"
							header = HTTPServerHelper.httpHeader(200,Map("Lock-Token" ->("opaquelocktoken:" + lock_id),"Content-Type" -> "text/xml; charset=utf-8"),response.length)
						}
					}
				}
			

				case _ => {
					//Yes: does the lock already exist?
					isLocked(fullkey) match {
						
						case true => {header = HTTPServerHelper httpHeader(423,"text/plain","")}
						
						case false => {
							debug("fullkey to be locked is " + fullkey)
							//- Copy the body for reuse in response
							val xml = XML loadString(new String(xmlbody))
							debug("xml as string: " + xml)
							val response_copy = ((xml child).mkString).trim
							debug("response copy: " + response_copy)
							//- set and generate lock for resource or tree
							val lock_id = (lock(fullkey,response)) get;
							//This is for Win7 Explorer
							//We need to be able to map a lock back to user in case of a request WITH lock but without authentication
							
							lock2User addLock(lock_id,ctx.user)
							//Set the lock data like owner etc for future reference
							//setLockData(fullkey,lock_id,response)
							//generate response
							response += """<?xml version="1.0" encoding="utf-8" ?> <D:prop xmlns:D="DAV:"><D:lockdiscovery><D:activelock> """
							response += response_copy
							response += "<D:timeout>Second-600</D:timeout>"
							response += "<D:locktoken><D:href>opaquelocktoken:" + lock_id + "</D:href></D:locktoken>"
							response += "</D:activelock></D:lockdiscovery></D:prop>"
							header = HTTPServerHelper.httpHeader(200,Map("Lock-Token" ->("opaquelocktoken:" + lock_id),"Content-Type" -> "text/xml; charset=utf-8"),response.length)
						}
					}
				}
			}
			data
		}
		
		override def <|(): Array[Byte] = {
			ctx.phase = 'allwritten
			debug(header)
			debug(response)
			(header ++ response) getBytes		
			
		}
				
		
	}
	
	
}
