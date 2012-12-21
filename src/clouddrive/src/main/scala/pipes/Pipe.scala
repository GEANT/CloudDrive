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

import java.io.{InputStream,FileOutputStream,File}
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.httpsupport._
import net.vrijheid.clouddrive.providers._
import net.vrijheid.clouddrive.providers.aws._
import net.vrijheid.clouddrive.providers.filesystem._
import net.vrijheid.clouddrive.config._
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes {

import providers.pithosplus.PithosPlusFileSystem

//
	//combine PipeItems and manage execution
	class Pipe[T] (source : PipeSource,sink : PipeSink)(implicit ctx : RootContext[T]) extends Treatise  with Serializable {
				
		private var pipe : List[PipeItem] = List[PipeItem]()
		
		def ++(phase : PipeItem) = {
			
			pipe = pipe :+ phase
		}
		
		def ||() {
			source ||();
			pipe foreach { element => {element || }}
			sink ||();
		}
		
		def >|() {
			//Push data from the source through the pipe
			sink >| (pipe.foldLeft(source >|) {(data,next) => { next >| data}})
		}
		
		def <|() {
			//Push data from sink to source back
			source <| (pipe.foldRight(sink <|){ (next,data) => { next <| data}})
		}
		
		def |>|() {
			sink |>| (pipe.foldLeft(source |>|) {(data,next) => { next |>| data}})
		}
		
		def |<|() {
			source |<| (pipe.foldRight(sink |<|){ (next,data) => { next |<| data}})
		}
		
		//Process this Pipe completely
		//The Thread.sleep(0) is there to act as a yield and prevent 
		// the i/o, encryption etc to have one thread eat too much CPU
		def process() {
			Thread.sleep(0)
			debug("Processing pipe")
			ctx.phase = 'initincoming
			||
			debug("|| done initializing")
			ctx.phase = 'incoming
			do { 
				>|();
				Thread.sleep(0)
			} while (ctx.phase == 'incoming)
			debug(">| done incoming")
			
			|>|
			debug("|>| done inciming commit")
			
			ctx.phase = 'outgoing
			do {
				<|();
				Thread.sleep(0)
			} while (ctx.phase == 'outgoing)
			
			debug("<| done outgoing")
			
			|<|
			debug("|<| done outgoing commit")
			
			ctx.phase = 'done
		}
	}
	
	object Pipe extends Debug {
		
		def mkMeterer[T](s : String)(implicit ctx: RootContext[T])  = synchronized {
			debug("####################")
			debug("mkMeterer for: " + s)
			s match {
				case s: String if s == "voldemort" => {
					new Metering()
				}
				case _ => {new IdentityItem()}
			}
			
		}
		
		def mkLogger[T](s : String)(implicit ctx: RootContext[T]) = synchronized {
			
			s match {
				case _ => {new Logger()}
			}
		}
		    		    

		
		def mkEncryption[T](kind : String)(implicit ctx: RootContext[T]) = synchronized {
			kind match {
				case s: String if s == "aes" => new Encryption()
				case _ => new IdentityItem()
			}
		}
		
		def mkDecryption[T](kind : String)(implicit ctx: RootContext[T]) = synchronized {
			kind match {
				case s: String if s == "aes" => new Decryption()
				case _ => new IdentityItem()
			}			
		}
		
		def mkCompressor[T](kind : String)(implicit ctx: RootContext[T]) = synchronized {
			kind match {
				case s: String if s == "gzip" => new Compressor()
				case _ => new IdentityItem()
			}
		}
		
		def mkDecompressor[T](kind : String)(implicit ctx: RootContext[T]) = synchronized {
			kind match {
				case s: String if s == "gzip" => new Decompressor()
				case _ => new IdentityItem()
			}			
		}
		
		def mkVersioning[T](kind : String)(implicit ctx: RootContext[T]) = synchronized {
			kind match {
				case _ => new Versioning()
			}			
		}
		
		def mkQuota[T]()(implicit ctx: RootContext[T]) = { 
			Config("authnMethod","static") match  {
				case s: String if s == "voldemort" => {new Quotas}
				case _ => {new IdentityItem}
			}
		}
	}
	
	object Storage extends Debug {
		
		def mkStorage[T](kind : String,key : String)(implicit ctx : RootContext[T]) : Storage = synchronized {
			
			kind match {
				
				case "filesystem" => {new FileSystemStore(key) }
				case "s3" => {new AWSFileSystem(key) }
        case "pithosplus" => new PithosPlusFileSystem(key)
				case _ => {new FileSystemStore(key)}
			}
			
		}
	}

	object PipeUtils extends Treatise {
		

		//Converts data from an inputstream to a file ready for storage in the cloud on behalf of a user. 
		//Creates the resource in the metadata store (if necessary) and sets the crypto keys
		def in2CloudFile[T](in: InputStream,out: File)(implicit ctx: RootContext[T]): Unit = synchronized {
						
			val metadata = new MetaData[T]()			
			val fullpath = metadata.followLink(stripTrailingSlash("/"+ctx.user+ctx.verb.header.getOrElse("resource","/")))
			val encryptor = new Encryption
			val compressor = new Compressor
			encryptor ||;
			compressor ||;
			
			//Set this up for the encryption/compression classes. It's an anonymous class that matches
			//the structural type signature of the filter in in2out in utils.Utils
			val file_2_cloud_filter = new {
				
				//This is a simple "stream" filter
				def filter(data: Array[Byte]): Array[Byte] = {
					encryptor >|(compressor >|(data))
				}
				
				//The wrap up is a bit more involved, as it needs to close the encryption and store the keys
				//in the metadata store.
				def wrapUp(data: Array[Byte]): Array[Byte] = {
					val ret = encryptor |>|(compressor|>|(data))
					//CHeck to see if the respurce exists
					//Otherwise create it, because we need to store the key and IV for the crypto part
					encryptor.exists(fullpath) match {
						case true => {}
						case false => {
							encryptor createResource(fullpath)
							//Otherwise the guids in the filesystems are null
							encryptor setOriginal(fullpath,UUID)
						}
					}
					//CLose the encryption so all IV and crypto keys get stored in the metadata store
					encryptor |<|(Array[Byte]())
					//return the final piece of data
					ret
				}

			}
			
			val fileout = new FileOutputStream(out)
			in2out(in,fileout,file_2_cloud_filter,ctx.actualContentLength,16384)
			
		}
		
	}
}