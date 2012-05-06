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
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.httpsupport._
import voldemort.client._
import java.io.{Serializable}

package net.vrijheid.clouddrive.control {
	
	//
	@SerialVersionUID (20111802L)
	trait Context  extends Serializable  {
		
	}
	
	//
	@SerialVersionUID (20111803L)
	class RootContext[T](vrb: HTTPVerb[T]) extends Context  with Serializable {
		
		var verb = vrb
		
		@transient val zk = Config("metadata_store","") match {
			//Legacy
			//case "zookeeper" => {new ZooTalk()}
			case _ => { Config("authnMethod","") match {
				//Legacy
				//case "zookeeper" => {new ZooTalk()}
				case _ => {null}
			}}
		}
		
		//Legacy
		//Ugly, selective init
		//if (!(zk == null)) {zk loadConfig}
		//Console println("In Context, zk initialized to " + zk) 				
		@transient var store : Storage = _ 
		var user : String = _
		/** phase is used to denote the state the over alld ata processing is in - this is useful for the way we log, encrypt, etc.
		Phases are: 
		initincoming - initialize for receiving data
		incoming - reading incoming data
		allread - all incoming data has been read
		initoutgoing - initialize for outgoing data
		outgoing - writing outgoing data
		allwritten - all data has been written
		done - finished writin response
		
		States are typically changed in the WebDAV command sink, or on initial Pipe creation
		*/
		var phase : Symbol = 'initincoming
		//This can be used for per-user configuration
		var userConfig : Config = _
		//This is the  data that length that has been actualyy stored after compression and encryption, or anything else that changes the content length
		var putContentLength: Long = 0
		//This is the actual (or original) content length; needed for HTTP headers and such
    	var actualContentLength: Long = 0
		//In rare cases, we need to bypass and send a HTTP code directly, This is our hook or that
    	@transient var bypass: HTTPServerHelper = _
		//If the full metatdata path is not directly deducable from the http resource (mostly when using duplicates in the common folder)
		//this function can be set by the part that changes the metadata path so that parts of the program can read it out correctly.
		//By default, return the same value
		@transient var recomputeFullPath: (String) => String = (s: String) => s
		@transient var storageclient: VMClient[String,VMNode] = _
		@transient var owner: String = _
		
	}	
	
	//
	@SerialVersionUID (20111804L)
	class WebDAVContext(vrb: HTTPVerb[java.net.Socket]) extends RootContext(vrb)  with Serializable
	
	//
	@SerialVersionUID (20111805L)
	class LiftContext(vrb: HTTPVerb[String]) extends RootContext(vrb)  with Serializable
	
	class TestContext(vrb: HTTPVerb[String],testuser: String) extends RootContext(vrb) with Serializable {
		
		user = testuser
		verb = vrb
	}
	
}
