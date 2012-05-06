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
import net.vrijheid.clouddrive.httpsupport._
import net.vrijheid.clouddrive.control._
import java.io.{IOException}

package net.vrijheid.clouddrive.pipes {
	
	class BaseSocketSource(helper : HTTPServerHelper)(implicit ctx : WebDAVContext) extends PipeSource {
		
		debug("Entering BaseSocket constructor")
		private var content_read: Long = 0	
		protected var content_length = ((ctx.verb.header.getOrElse("Content-Length","0")).trim).toLong
		debug("BaseSocket constructor called")
				
		override def||() {
			
			//This prevents our Thread from needlessly writing to a closed stream (it does that on Linux at least
			//The exception is thrown to unwind the stack
			if(helper isInputShutdown) {throw new IOException }
			
			ctx.actualContentLength = content_length
			ctx.putContentLength = ctx.actualContentLength
		}
		
		//Push outgoing data
		override def <| (data : Array[Byte]) { 
			
			//This prevents our Thread from needlessly writing to a closed stream (it does that on Linux at least
			//The exception is thrown to unwind the stack
			if(! (helper socketIsOpen)) {throw new IOException }
			
			debug("BaseSocket <|")
			if(! (data isEmpty)) {
				//try { helper write data} catch { case e: Exception => {}}
				helper write data
			}
		}
		//"commit" outgoing stream (finalize action)
		override def |<|(data: Array[Byte]) {
			if(! (helper socketIsOpen)) {throw new IOException }
			
			if(! (data isEmpty)) { 
				try { helper write data} catch { case e: Exception => {}}
			}
			debug("BaseSocket |<|")
			try { helper close } catch { case e: Exception => {}}
			try { ctx.store close } catch { case e: Exception => {}}
			//Legacy
			//try { if (!(ctx.zk == null)) {ctx.zk close}} catch { case e: Exception => {}}
		}	

				
	}
		
	class DataSocketSource(helper : HTTPServerHelper)(implicit ctx : WebDAVContext) extends BaseSocketSource(helper)  {

		private var buffer = Array[Byte]()
		private var content_read: Long = 0	
		//the toLowerCase fixes a bug from OS X
		var encoding = ((ctx.verb.header.getOrElse("Transfer-Encoding","not chunked")).trim).toLowerCase
		debug("encoding = " + encoding)

		
		//Push data incoming
		override def >| (): Array[Byte] = {
			//This prevents our Thread from needlessly writing to a closed stream (it does that on Linux at least
			//The exception is thrown to unwind the stack
			if(!(helper socketIsOpen)) {throw new IOException }

			debug("In DataSocketSource >|")
			/**if (ctx.verb.header.contains("X-Expected-Entity-Length")) {
				debug("X-Expected-Entity-Length found, proceding as if not chhunked")
				content_length = (ctx.verb.header("X-Expected-Entity-Length")) toInt;
				ctx.actualContentLength = content_length
				encoding = "not chunked"
			}*/

			var chunked_content_read = -1

			
			val newdata =  encoding match {
				
				case "chunked"  => { 
					//This is set to prevent erroneous exits later in ths method
					content_length = -1
					debug("chunked encoding")
					val data = (helper readChunk)
					//Should be: no more chunks
					chunked_content_read = (data length)
          			//Add the chunked_content_read length so our actual content_length is accurate
          			ctx.actualContentLength += chunked_content_read
					debug("actualContentLength read = " + (ctx.actualContentLength).toString)
					data
				}	

				case "not chunked" => {
					//Read all content in chunks of 16K, return 16K once we have it, empty otherwise
					debug("NO chunked encoding")
					var remaining: Long = 16384
					//If we have to  read less than 16K, adjust remaining
					if ((content_length - content_read) < 16384) {
						remaining = (content_length - content_read)
					}
					debug("content_read = " + (content_read toString))
					debug("content_length = " + (content_length toString))
					debug("remaining = " + (remaining toString))
					//Read a chunk of data
					val data = (helper readChunk(remaining))
					//Add to content-read
					content_read = content_read + (data length).asInstanceOf[Long]
					debug("Data length: " + ((data length) toString))
					debug("Content read: " + (content_read toString))
					debug(content_read toString)
					data
				}
			}
			
			//Append to the buffer
			buffer = buffer ++ newdata
			
			
			//First expression covers, content-length, second covers chunked encoding
			((content_read == content_length) || (chunked_content_read == 0)) match {
				//All data has been read
				case true => {
					debug("allread")
					ctx.phase = 'allread
					buffer
				}
				//Still data to go, see if we have a slice of 16 ready
				case false => {
					debug("More to read")
					//If we have more than 16K, slice and return the 16K
					if (buffer.length >= 16384) {
						debug("returning 16K data slice")
						val next = buffer slice(0,16384)
						buffer = buffer slice(16384,buffer.length)
						next
					}
					//Not yet...
					else {
						debug("Returning nothing")
						Array[Byte]()
					}
				}
			}
		}	
	}
	
	
	
	class CommandSocketSource(helper : HTTPServerHelper)(implicit ctx : WebDAVContext) extends BaseSocketSource(helper) {


		override def >| (): Array[Byte] = {
			
			//This prevents our Thread from needlessly writing to a closed stream (it does that on Linux at least
			//The exception is thrown to unwind the stack
			
			if(!(helper socketIsOpen)) {throw new IOException }

			debug("Command, entering >|")
			//Simply read all content all-at-once, based on the verb content length, then return
			if (0 == content_length) {
				debug("no command data")
				ctx.phase = 'allread
				Array[Byte]()
			}
			else {
				val encoding = ((ctx.verb.header.getOrElse("Transfer-Encoding","not chunked")).trim).toLowerCase;
				debug("encoding: " + encoding)
				var newdata =  encoding match {
					case "chunked" => {
						var alldata = Array[Byte]() 
						var buffer = Array[Byte]()
						//Read all chunks
						do {
							buffer = helper readChunk;
							alldata = alldata ++ buffer
						} while (!((buffer length) == 0))
						alldata
					}
					
					case "not chunked" => {
						//Read all content 
						debug("Let's read all content")
						helper readChunk(content_length)
					}
				}		
				ctx.phase = 'allread
				newdata		
			}
		}
	}		
}