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
import java.io.{InputStream,OutputStream,FileInputStream,FileOutputStream,BufferedOutputStream,File}
import javax.net.ssl._
import javax.net._
import java.security.MessageDigest
import java.util.{List => JList,_}


package net.vrijheid.clouddrive.client {
	
	class SimpleClient(hostname: String, user: String, password: String,val ssl: Boolean = true) {

		val debug_mode = false;
	
		Authenticator.setDefault(new Authenticator(){
		    protected  override def getPasswordAuthentication(): PasswordAuthentication = {
		        return new PasswordAuthentication(user,password.toCharArray());
		    }});
	
	
		def debug(msg : String) {
			if (debug_mode) {
				scala.Console println(msg)
			}
		}
	
		def debug(value : AnyRef) {
			if (debug_mode) {value}
		}

		val crlf = "\r\n"

		//Remove the " from a string
		def stripQuotes(text : String) = {
			text replaceAll("\"","")
		}


		def bytes2Hex(bytes: Array[Byte]) = bytes.map(b => "%02X".format(b)).mkString

		//Handy for HTTP Digest authN
		def generateNonce() = {
			val r = new Random()
			var bnonce = new Array[Byte](16)
			r.nextBytes(bnonce)
			(bytes2Hex(bnonce)) toLowerCase
		}
		//I was baflled that Java doesn't have simple functions to do simple things, like md5. sha1 etc.
		//This helps.
		def md5(data: String): String = {
		  md5(data.getBytes("iso-8859-1"))
		}
		def md5(data: Array[Byte]): String = {
		  val md5 = MessageDigest.getInstance("MD5")
		  md5 update data
		  bytes2Hex(md5 digest ())
		}

		def computeHA1(user : String,realm : String,password : String) : String = {
			(md5(user + ":" + realm  + ":" + password)) toLowerCase
		}
	
		def computeHA2(verb : String, uri: String) = {
			(md5(verb + ":" + uri)) toLowerCase
		}
	
		def in2out(in: InputStream,out: OutputStream,length: Long,bufsize: Int = 16384) {
		
			//Some bookkeeping variables
			var remaining : Long = length
			var buf = new Array[Byte](bufsize)
			val buf_out = new BufferedOutputStream(out)
		
			//The copy loop
			do {
				val numread = in.read(buf,0,buf.length)
				buf_out write(buf,0,numread)
				remaining -= numread
			} while( remaining > 0)
			//flush and close our output
			buf_out flush;
			//buf_out close;
		}
		
		def getSocket(): Socket = {
		
			 val port = ssl match {
				case true => {443}
				case false => {8080}
			}
		
			val factory = ssl match {
				case true => {SSLSocketFactory getDefault}
				case false => {SocketFactory getDefault}
			}
		
			val socket = factory createSocket(hostname,port)
			if(ssl) {(socket.asInstanceOf[SSLSocket]).startHandshake}
			socket
		
		}	

		
		def readChunk(socket: Socket,terminator : String) : Array[Byte] = {
		
			//as integer sequence
		 	val int_seq = for (s <- terminator) yield (s toInt)
			//As List[Int]
		 	val list_crlf = int_seq toList
			//reverse the list for when we do I/O
		 	val reverse_list = (int_seq toList).reverse
			//HTTP Header end marker
		 	val http_header_marker = reverse_list ++ reverse_list
			val is = socket getInputStream

			//Initialize the data List
			var data : List[Int] = List();
			//read until we find the terminator, typically the List[Int] of crlf
			do {
				//read and prepend
				data = is.read() :: data
		
			} while  (!data.startsWith(terminator))
			//return data reversed (in order, as we prepend), converted to bytes
			val http_header_byte_seq = for {s <- (data reverse)} yield (s toByte)
			http_header_byte_seq toArray;
		}
		
		//This returns the proper authZ header
		def authenticate(verb: String,resource: String): String = {		
		
			//Get HTTP header
			val challenge = "GET / HTTP/1.1" + crlf + "Host: " + hostname + crlf + crlf
			val socket = getSocket
			(socket getOutputStream).write(challenge getBytes)
			val	header = new String(readChunk(socket,crlf))	
			//Socket close, we got the header
			socket close;
			//Get the WWW-Authenticate line
			val authn_line = (header split crlf).filter(x => x.contains("WWW-Authenticate:"))
			//Extract real and nonce
			val realm_index = header.indexOf("realm") + 6
			val nonce_index = header.indexOf("nonce") + 7
			debug(realm_index toString)
			debug((header.indexOf("\"",realm_index)) toString)
			val realm = stripQuotes(header.substring(realm_index,header.indexOf(",",realm_index)))
			val nonce = stripQuotes(header.substring(nonce_index,header.indexOf("\"",nonce_index) - 1))
			val opaque_index = header.indexOf("opaque") 
			val opaque = opaque_index match {
				case 0 => {generateNonce}
				case _ => {header.substring(opaque_index + 9,header.indexOf("\"",opaque_index + 9))}
			}
			//Get some more suff
			val cnonce = generateNonce;
			val nc = "0000001"
			val ha1 = computeHA1(user,realm,password)
			val ha2 = computeHA2(verb,resource)
			debug("response: " + stripQuotes(ha1 + ":" + nonce + ":" + nc + ":" + cnonce + ":" + "auth" + ":" + ha2))
			val response = (md5(stripQuotes(ha1 + ":" + nonce + ":" + nc + ":" + cnonce + ":" + "auth" + ":" + ha2))).toLowerCase
			//According to: see http://en.wikipedia.org/wiki/Digest_access_authentication, our response the authZ header
			"Authorization: Digest username=\"" + user + "\", realm=\"" + realm + "\", nonce=\"" + nonce + "\", uri=\"" + resource + "\", qop=auth, nc=" + nc + ", cnonce=\"" + 
			cnonce + "\", response=\"" + response + "\", opaque=\"" + opaque + "\"";
		}
	
		def getHeader(verb: String,resource: String,content_length: Long): String = {
		
			val local_resource = (resource startsWith "/") match {
				case true => {resource}
				case false => {"/" + resource}
			}
		
			val authz = authenticate(verb,resource)
			debug("authz = " + authz)
			verb + " " + local_resource + " HTTP/1.1" + crlf +
			"Content-Length: " + (content_length toString) + crlf +
			"Host: " + hostname + crlf +
			"Connection: close" + crlf +
			authz + crlf + crlf
		
		
		}
	
		def getConnection(verb: String,resource: String, content_length: Long): Socket = {
			val header = getHeader(verb,resource,content_length)
			val socket = getSocket;
			val out = socket.getOutputStream
			out.write(header getBytes)
			socket
		}
	
		def putFile(filename: String,resource_name: String) {
		
			val file = new File(filename)
			val length = file.length
			val socket = getConnection("PUT",resource_name,length)
			val in = new FileInputStream(file)
			val out = socket.getOutputStream;
			in2out(in,out,length)
			//Wait for the reply. TODO: handle status codes
			readChunk(socket,crlf)
			socket close;
		
		}
		
		def makeCollection(collection: String) {
			
			val local_collection = (collection startsWith "/") match {
				case true => {collection}
				case false => {"/" + collection}
			}
			
			val socket = getConnection("MKCOL",local_collection,0)
			readChunk(socket,crlf)
			//TODO: handle status codes
			socket close
		}
	
		def getFile(filename: String,resource: String) {
		
			val local_resource = (resource startsWith "/") match {
				case true => {resource}
				case false => {"/" + resource}
			}
		
			val proto = ssl match {
				case true => {"https://"}
				case false => {"http://"}
			}
		
			val url = (new URL(proto + hostname + local_resource))
			val connection = (url.openConnection()).asInstanceOf[HttpURLConnection];
			connection setRequestMethod "GET"
			connection setDoInput true
			val file = new File(filename)
			val in = connection.getInputStream
			val out = new FileOutputStream(file)
			in2out(in,out,connection getContentLength)	
		}
	
		def deleteFile(resource: String) {
		
			val local_resource = (resource startsWith "/") match {
				case true => {resource}
				case false => {"/" + resource}
			}
		
			val proto = ssl match {
				case true => {"https://"}
				case false => {"http://"}
			}
		
			val url = (new URL(proto + hostname + local_resource))
			val connection = (url.openConnection()).asInstanceOf[HttpURLConnection];
			connection setRequestMethod "DELETE"
			connection setDoInput true
			connection connect;
			val in = connection.getInputStream
			in close;
		}
		
		def deleteCollection(coll: String) = deleteFile(coll);
	}
}