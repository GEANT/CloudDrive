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

package net.vrijheid.clouddrive.httpsupport {

 	abstract class HTTPVerb[T] (val socket : T, val header : Map[String,String]) 

 	sealed abstract class WebDAVVerb (override val socket : Socket, override val header : Map[String,String]) extends HTTPVerb (socket,header) {
	}

	case class GET(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class PUT(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class HEAD(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class POST(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class DELETE(override val socket : Socket,override val  header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class MOVE(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class COPY(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class OPTIONS(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class MKCOL(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class PROPFIND(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class PROPPATCH(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class LOCK(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	case class UNLOCK(override val socket : Socket, override val header : Map[String,String]) extends WebDAVVerb(socket,header)
	//Note that the symbol can be changed, and indicates the action the website needs to take. This is to allow acces control checks i.e. for shared folders.
	//Symbol values supported by the authorization engine are:
	/*
	
	TBD- CODE/CC add valid symbol values for WEBSITE verb here!
	
	*/
	case class WEBSITE(override val header: Map[String,String],var action: Symbol) extends HTTPVerb[String]("empty",header)
	case class TEST(override val header: Map[String,String],var action: Symbol) extends HTTPVerb[String]("empty",header)
}