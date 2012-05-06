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
import java.net._
import scala.actors.Actor
import scala.actors.Actor._
import net.vrijheid.clouddrive.httpsupport._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.exceptional.{QuotaException}
import net.vrijheid.clouddrive.utils.{Debug}

package net.vrijheid.clouddrive {

	class WebDavHandler(socket: Socket) extends Runnable with Debug {
	
		private val this.socket = socket
		private val my_http_helper = new HTTPServerHelper(socket)
		private var http_processing : HTTPProcessor = _
		override def run () = {
			try {
				//Read a http header
				val verb = my_http_helper readHttpHeader()
				debug(verb.toString())
				debug(verb.header("resource"))
				debug(verb.header("verb"))
				//This will handle all processing I/O and logic, including authN 
				http_processing = new HTTPProcessor(verb,my_http_helper)
				http_processing process()
				try { my_http_helper close} catch { case _ => {}}
				debug("done  handling complete webdav request")
			} 
			catch {
				
				case qe: QuotaException => {
					//We somehow went past or quotum. Send a 404
					my_http_helper write((HTTPServerHelper httpHeader(404,"","text/html")) getBytes)
					try { my_http_helper close } catch { case e: Exception => {}}
					try { http_processing.ctx.store close } catch { case e: Exception => {}}
					//Legacy
					//try { if(!(http_processing.ctx.zk == null)) http_processing.ctx.zk close } catch { case e: Exception => {}}
					//try { if(!(http_processing.ctx.storageclient == null)) http_processing.ctx.storageclient.factory.close } catch { case e: Exception => {}}
				}
				
				case e : Exception =>{
					//We can comment this in in case of major embarrasing errors....
					try { my_http_helper close } catch { case e: Exception => {}}
					try { http_processing.ctx.store close } catch { case e: Exception => {}}
					//Legacy
					//try { if(!(http_processing.ctx.zk == null)) http_processing.ctx.zk close } catch { case e: Exception => {}}
					//try { if(!(http_processing.ctx.storageclient == null)) http_processing.ctx.storageclient.factory.close } catch { case e: Exception => {}}
					//e.printStackTrace;
				}
			}
			finally {
				try { my_http_helper close } catch { case e: Exception => {}}
				try { http_processing.ctx.store close } catch { case e: Exception => {}}
				//Legacy
				//try { if(!(http_processing.ctx.zk == null)) http_processing.ctx.zk close } catch { case e: Exception => {}}
				//try { if(!(http_processing.ctx.storageclient == null)) http_processing.ctx.storageclient.factory.close } catch { case e: Exception => {}}
			}
		}
		
		
		
	}


	//Companion object; defines some constants???
	object WebDavHandler {
	

	}

}
