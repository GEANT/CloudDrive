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
import java.io.{InputStream,OutputStream}
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.exceptional._

package net.vrijheid.clouddrive.httpsupport {

    class HTTPServerHelper(socket : Socket) extends Debug{
	
		val handy = new Handy()
		//Network / stream inits
		val is = socket getInputStream
		val os = socket getOutputStream
			
		val crlf = HTTPServerHelper.crlfs

		//as integer sequence
	 	val int_seq_crlf = for (s <- crlf) yield (s toInt)
		//As List[Int]
	 	val list_crlf = int_seq_crlf toList
		//reverse the list for when we do I/O
	 	val reverse_list_crlf = list_crlf reverse
		//HTTP Header end marker
	 	val http_header_marker = reverse_list_crlf ++ reverse_list_crlf	

		private def toByteArray(in_list : List[Int]) : Array[Byte] = {

			val byte_seq = for {some <- in_list} yield (some toByte)
			byte_seq toArray
		}
	
		private def stringFomIntList(in_list: List[Int]) : String = {
		
			val byte_array = this.toByteArray(in_list)
			val res = new String (byte_array)
			res
		
		}
		
		def read() : Array[Byte] = {
			debug("HTTPServerHelper in read()")
			val data : Array[Byte] = new Array[Byte](16384)
			val size = is read(data,0,0)
			debug("size read: " + (size toString))
			if (size == -1) {Array[Byte]()}
			else { data slice(0,size)}
		}
		
		def isInputShutdown():Boolean = {
			((socket isInputShutdown) || (socket isClosed))
		}
		
		def isOutputShutdown():Boolean = {
			((socket isOutputShutdown) || (socket isClosed))	
		}
		
		def socketIsOpen():Boolean = {
			 socket!=null && !socket.isClosed() && !socket.isInputShutdown() && !socket.isOutputShutdown();
		}

		def write(data : Array[Byte]) {
			os write(data,0,data length)
			os flush()
			//Hopefuly, this gives an I/O exception in case the connection is dropped
			//socket sendUrgentData(0)
		}
		
		def readChunk(size: Long): Array[Byte] = {
			readChunk(size.asInstanceOf[Int])
		}
		
		def readChunk(size : Int) : Array[Byte] = {
			
			debug("In readChunk,  " + (size toString))
			// initalize the data array
			var data : Array[Byte] = new Array(size)
			//Initialize loop variables
			var remaining  = size
			var received = 0
			var offset = size - remaining
		
			do {
				//read data here in chunks
				//received is how much we have read
				debug("about to receive")
				received = is read(data,offset,remaining)
				//Guard against broken connections
				if (received == -1) {
					if (this.socket.isClosed) {throw new BrokenConnectionException}
				}								
				//append read data to data array
				remaining -= received
				offset = size - remaining
				debug("************")
				debug("Offset : "  + offset toString)
				debug("Received : "  + received toString)
				debug("Remaining : " + remaining toString)
			} while (remaining > 0)
		
			data
		}
	
		def readChunk() : Array[Byte] = {
			//First read the size of the chunk and get it as integer
			//http://en.wikipedia.org/wiki/Chunked_transfer_encoding#Format
			//Strip CRLF after the data!
			val size_as_int_list : List[Int] = this.readChunk(this.reverse_list_crlf) 
			val size_as_string = (this stringFomIntList(size_as_int_list)) trim;
			debug("### size_as_string for chunk is " + size_as_string)
			//From Hex value to int
			val size : Int = Integer.parseInt(size_as_string,16);
			size match {
				//Last Chunk
				case 0 => {Array[Byte]()}
				//Still working
				case n: Int => {
					//And read the data
					val data = readChunk(n)
					//Read the trailing crlf
					readChunk(2)
					data
				}
			}
		}
		
		def readChunk(terminator : List[Int]) : List[Int] = {
	
			val is = this.socket getInputStream
	
			//Initialize the data List
			var data = List[Int]() 
			//read until we find the terminator, typically the List[Int] of crlf
			do {
				//read and prepend
				data = is.read() :: data
			
			} while  (!data.startsWith(terminator))
			//return data reversed (in order, as we prepend)....
			data reverse
		}
	
		def readHttpHeader() = {
			//This looks not to efficient, but should work in lots of cases, which counts for something
			//We get a list of ints back
			//val http_header_int_list = this readChunk(http_header_marker)
			//Convert them to bytes so we can make a string and back to list later
			val http_header_byte_seq = for {s <- this.readChunk(http_header_marker)} yield (s toByte)
			//We split it on a line by line basis (could do this directly?)
			var http_header_array = (new String(http_header_byte_seq toArray)) split crlf
			//The first line (verb etc.) is diffferently
			val first_line = http_header_array(0).split(" ")
			//Start building or HTTP header "map"
			var http_header_map = Map("verb" -> first_line(0), "resource" -> URLDecoder.decode(first_line(1)))
			//drop the first line now
			val http_header_list : List[String] = http_header_array toList
			val http_header_list_2 : List[String]  =  http_header_list drop 1
			//reverse to put it back in order
			val http_header_list_3 : List[String]  = (http_header_list_2 reverse)
			//for every line add header field/value to map
			http_header_list_3 foreach { 
				line : String =>
				//split on ":", the HTTP key/value def
				val hdr_arr : Array[String] = line split ":"
				//Get the value
				var hdr_val = hdr_arr(1)
				//Boundary case: if there is a ':' in the HTTP value, we have splitted on that. Rejoin it.
				if (( hdr_arr length) > 2 ) {
					//Execute the join (reduceLeft) on all but the first 
					hdr_val = (hdr_arr drop 1) reduceLeft(_ + ":" + _)
				}
				// Add entry to Map, and add the header_length for metering bookkeeping
				http_header_map = http_header_map ++ Map (hdr_arr(0) -> hdr_val,"header_length" -> (http_header_byte_seq length).toString)	
				//We return nothing 			
				()
			}

			//match the HTTP VERB and create the corresponding case class instance
			val verb_object = http_header_map("verb") match {
			
				case "GET" => GET(this.socket,http_header_map)
				case "PUT" => PUT(this.socket,http_header_map)
				case "HEAD" => HEAD(this.socket,http_header_map)
				case "POST" => POST(this.socket,http_header_map)
				case "COPY" => COPY(this.socket,http_header_map)
				case "MOVE" => MOVE(this.socket,http_header_map)
				case "DELETE" => DELETE(this.socket,http_header_map)
				case "MKCOL" => MKCOL(this.socket,http_header_map)
				case "OPTIONS" => OPTIONS(this.socket,http_header_map)
				case "LOCK" => LOCK(this.socket,http_header_map)
				case "UNLOCK" => UNLOCK(this.socket,http_header_map)
				case "PROPFIND" => PROPFIND(this.socket,http_header_map)
				case "PROPPATCH" => PROPPATCH(this.socket,http_header_map)
				case _ => GET(this.socket,http_header_map)
			}
			verb_object
		}		
		
		def sendAuthHeader(socket: Socket) {

			//Send WWW-Authenticate header
			val crlf = "\r\n"
			val realm = Config("realm","beneathclouds.com")
			val nonce = handy generateNonce
			val now : String = handy idateNow
			
			val content = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"  \"http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd\"	<HTML><HEAD><TITLE>Error</TITLE><META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=ISO-8859-1\"></HEAD><BODY><H1>401 Unauthorized.</H1></BODY></HTML>"
			val contentlength = (content length) toString

			//Create the response
			val wwwauthn = "HTTP/1.1 401 Unauthorized" + crlf + "Server: Clouddrive/7" + crlf + "Date: " + now + crlf + "WWW-Authenticate: Digest realm=" + realm + ",qop=\"auth\",nonce=\"" + nonce + "\"" + crlf + "Content-Type: text/html" + crlf + "Content-length: " + contentlength + crlf + "Connection: close" + crlf + crlf + content
			//val wwwauthn = "HTTP/1.1 401 Unauthorized" + crlf + "Server: Clouddrive/7" + crlf + "Date: " + now + crlf + "WWW-Authenticate: Digest realm=" + realm + ",qop=\"auth\",nonce=\"" + nonce + "\"" + crlf + "Content-Type: text/html" + crlf + "Connection: close" + crlf + crlf 

			//Send response for authentication via socket
			os write (wwwauthn getBytes)
			os flush;
			debug(wwwauthn)
			socket close
			
		}
		
		def writePage(page : String) {
			writePage(page getBytes)
		}
		
		def writePage(page : Array[Byte]) {
			os write page
		}
		
		def close() {
			os.flush();
			os.close();
		}
		
	}
	
	object HTTPServerHelper extends Treatise {
		
		//HTTP constants	
		val crlfs = "\r\n"
		val httpcodes = Map (
			102 -> "Processing",
			200 -> "OK",
			201 -> "Created",
			204 -> "No Content",
			207 -> "Multi-Status",
			400 -> "ERROR",
			404 -> "Not Found",
			409 -> "Conflict",
			422 -> "Unprocessable Entity",
			423 -> "Locked",
			424 -> "Failed Dependency",
			500 -> "Internal Server Error",
			507 -> "Insufficient Storage"
		)
		
		def httpHeader(statuscode : Int, contenttype : String, content : String) : String = synchronized {
			httpHeader (statuscode,contenttype,content length)
		}
		
		def httpHeader(statuscode : Int, contenttype : String, content : Array[Byte]) : String = synchronized {
			httpHeader (statuscode,contenttype,content length)	
		}
		
		def httpHeader(statuscode : Int, contenttype : String, contentlength: Int) : String = synchronized {
			httpHeader(statuscode,Map("Content-Type" -> contenttype),contentlength)
		}
		
		def httpHeader(statuscode : Int, contenttype : String, contentlength: Long) : String = synchronized {
			httpHeader(statuscode,Map("Content-Type" -> contenttype),contentlength)
		}

		def httpHeader(statuscode : Int, headerfields : Map[String,String], contentlength: Int) : String = synchronized {
			
			val codeword = httpcodes(statuscode)
			var response = "HTTP/1.1 " + (statuscode toString) + " " + codeword + crlfs + "Server: Clouddrive 7" + crlfs + "Date: " + idateNow + crlfs 
			headerfields foreach {
				(field) => {
					response += ((field _1 ) + ": " + (field _2) + crlfs)
				}
			}
			response += "MS-Author-Via: DAV" + crlfs
			response += "Connection: close" + crlfs
			response + "Content-length: " + contentlength + crlfs + crlfs
			
		}
		
		def httpHeader(statuscode : Int, headerfields : Map[String,String], contentlength: Long) : String = synchronized {
			
			val codeword = httpcodes(statuscode)
			var response = "HTTP/1.1 " + (statuscode toString) + " " + codeword + crlfs + "Server: Clouddrive 7" + crlfs + "Date: " + idateNow + crlfs 
			headerfields foreach {
				(field) => {
					response += ((field _1 ) + ": " + (field _2) + crlfs)
				}
			}
			response += "MS-Author-Via: DAV" + crlfs
			response += "Connection: close" + crlfs
			response + "Content-length: " + contentlength + crlfs + crlfs
			
		}
	}
}
