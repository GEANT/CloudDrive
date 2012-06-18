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
import java.io.{Serializable}
import net.vrijheid.clouddrive.utils.Utils
import net.vrijheid.clouddrive.httpsupport._
import net.vrijheid.clouddrive.pipes._
import net.vrijheid.clouddrive.pipes.webdavcmds._
import net.vrijheid.clouddrive.providers._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.utils._

package net.vrijheid.clouddrive {
		
	 //
	 class HTTPProcessor(verb : HTTPVerb[java.net.Socket], helper : HTTPServerHelper) extends Treatise  with Serializable  {
		
		//use an abstract type variable (because we can ;-)
		type Authentication = () => Boolean
		var authenticator : Authentication = _
		var pipe : Pipe[java.net.Socket] = _
		
		//Set up initial context
		implicit val ctx = new WebDAVContext(verb)
    	ctx.bypass = helper
				
		//Initialize processing based on config, use appropiate subclass/trait for execution
		def initProcessing() {
			//setup authentication
			if (Config("authnMethod") != ""){
				this.authenticator = WebDavAuth getAuthenticator(Symbol(Config("authnMethod").toLowerCase()),verb.header)	
			} else { this.authenticator = WebDavAuth getAuthenticator('deny,verb.header) }
			
		}
	
		lazy val storageClient = VMTalk getStorageClient;
		
		//NOTE: temporary hack as long as we only use Voldemort
		/* (Correct) behaviour: if it's a link, return the resolved link
		   Otherwise: return the original path as it isn't a link
		
		def followLink(path: String): String = {
			storageClient getValue(path) match {
				//Note: we translate this to the VMkey, the destination is relative
				case link: VMLink => {
					link.destination _2 match {
						case l: VMLink => {followLink(l.destination _1)}
						case _ => {link.destination _1}
					}
				}
				//No link, just return the original path
				case _ => path
			}			
		}
		*/
			
		//Based on Verb, dispatch to handler˜
		def process() {
			initProcessing
			//authenticate
			if (! authenticator()) {
				//Send authN request header back
				helper.sendAuthHeader(verb.socket)
				helper close
			} else {
				//Authenticated, continue
				debug("authenticated")
				//Note that user has been set magically by the authenticator inside the context
				ctx.userConfig = Config.userConfig(ctx)
				
				//FORWARD PORT for other metadata stores - looks like this isn't going to happen for Zookeeper, ZK is too limited (memory only, small # files)
				//Translate any VMLinks right here
				//We'll use the "res" value to fool the storage layer
				ctx.storageclient = storageClient
				val fullpath = "/" + ctx.user + ctx.verb.header("resource")
				val md = MetaDataFactory(ctx.userConfig("metadata_store"))
				/*val res = storageClient getValue(fullpath) match {
					case link: VMLink => link.destination _1
					case _ => ctx.verb.header("resource")
				}*/
				
				val res = md.followLink(fullpath)
				
				//CODE_CC: at this point we have the FS pointer, set the owner
				//Now we can follow a link IF it is a shared file, and determine the owner
				//ctx.owner = followLink(fullpath).getUserNameFromPath
				
				
				//Set the Storage Engine based on user config, as well as their metadata client
				ctx.store = Storage.mkStorage(
					ctx.userConfig("storage"),
					res
				)
				debug(ctx.store)
				val pipe = (verb: @unchecked) match {
					
					//add various sorts of Quota mgt elements to pipes
					//Mostly, PUT,COPY,POST,GET
					
					//Pipe setup here per verb based on user config
					case GET(socket,header) => {
						val pipe = new Pipe(new CommandSocketSource(helper), new GETSink()(ctx))
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe ++ Pipe.mkDecompressor(ctx.userConfig("compression","gzip"))
						pipe ++ Pipe.mkDecryption(ctx.userConfig("encryption","aes"))
						pipe
					} 
					
					case PUT(socket,header) => {
						val pipe = new Pipe(new DataSocketSource(helper), new PUTSink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe ++ Pipe.mkVersioning(ctx.userConfig("versioning","default"))
						pipe ++ Pipe.mkCompressor(ctx.userConfig("compression","gzip"))
						pipe ++ Pipe.mkEncryption(ctx.userConfig("encryption","aes"))
						pipe
					}
					
					case HEAD(socket,header) => {
						val pipe = new Pipe(new CommandSocketSource(helper), new HEADSink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe
					}
					
					case POST(socket,header) => {
						//val pipe = new Pipe(new DataSocketSource(helper), new POSTSink())
						val pipe = new Pipe(new DataSocketSource(helper), new PUTSink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe ++ Pipe.mkVersioning(ctx.userConfig("versioning","default"))
						pipe ++ Pipe.mkCompressor(ctx.userConfig("compression","gzip"))
						pipe ++ Pipe.mkEncryption(ctx.userConfig("encryption","aes"))
						pipe
												
					}
					
					case DELETE(socket,header) => {
						val pipe = new Pipe(new CommandSocketSource(helper), new DELETESink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe
					}
					
					case MOVE(socket,header) => {
						val pipe = new Pipe(new CommandSocketSource(helper), new MOVESink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe
					}
					
					case COPY(socket,header) => {
						val pipe = new Pipe(new CommandSocketSource(helper), new COPYSink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe
					}
					
					case OPTIONS(socket,header) => {
						debug("In OPTIONS case")
						val pipe = new Pipe(new CommandSocketSource(helper), new OPTIONSSink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe
					}
					
					case MKCOL(socket,header) => {
						val pipe = new Pipe(new CommandSocketSource(helper), new MKCOLSink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe
					}
					
					case PROPFIND(socket,header) => {
						debug("In PROPFIND case")
						val pipe = new Pipe(new CommandSocketSource(helper), new PROPFINDSink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe
					}
					
					case PROPPATCH(socket,header) => {
						val pipe = new Pipe(new CommandSocketSource(helper), new PROPPATCHSink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe
					}
					
					case LOCK(socket,header) => {
						val pipe = new Pipe(new CommandSocketSource(helper), new LOCKSink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe
					}
					
					case UNLOCK(socket,header) => {
						val pipe = new Pipe(new CommandSocketSource(helper), new UNLOCKSink())
						pipe ++ Pipe.mkLogger(ctx.userConfig("logging","default"))
						pipe ++ Pipe.mkMeterer(ctx.userConfig("metering","default"))
						pipe ++ Pipe.mkQuota();
						pipe
					}
									
				}
				debug("Pipe ready to process")
				pipe process;
			}
			()
		}
	
	}
	
	
}
