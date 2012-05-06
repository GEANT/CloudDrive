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
import net.vrijheid.clouddrive.providers.{MetaData}
import net.vrijheid.clouddrive.config._
import javax.crypto._
import javax.crypto.spec._
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes {
	
	//
	class Encryption[T](implicit val ctx: RootContext[T])  extends MetaData[T] with PipeItem with Serializable  {
		
		//Initialize a Cipher
		private val cipher = Cipher getInstance "AES/CBC/PKCS5Padding"
		private val keygen = KeyGenerator getInstance "AES"
		private val secretkey = keygen generateKey
		private val raw = secretkey getEncoded
		private val secretkeyspec = new SecretKeySpec(raw,"AES")
		cipher init(Cipher ENCRYPT_MODE,secretkeyspec)
		private val IV = cipher getIV
		private var fullpath = ""
		private var encoding = ""
		
		
		override def ||()  {
			
			//We need to know if the encoding is chunked, in order to chose a stratgey for adapting the actual and put content lengths
			encoding = ((ctx.verb.header.getOrElse("Transfer-Encoding","not chunked")).trim).toLowerCase
			//We need to reset te putContentLength to zero, because we'll be adding the real block sizes dynamically to it.
			ctx.putContentLength  = 0
			//Get the path including symlink
			fullpath = followLink(stripTrailingSlash("/" + ctx.user + ctx.verb.header("resource")))			
		}
		
		private def modPutContentLength(put: Int) {
			ctx.putContentLength += put
		}
		
		override def >|(data: Array[Byte]) = {
			var retdat = cipher update data
			if(retdat == null) {retdat = Array[Byte]()}
			//Change the actual and put content length on the fly			
			modPutContentLength(retdat length)
			retdat
		}
		
		override def |>|(data: Array[Byte]) = {
			val retdat = cipher doFinal(data)
			//Change the put content length on the fly
			modPutContentLength(retdat length)
			//If we get no data (zero byte files) the encrypted block is the minimum blocksize, i.e. 16
			if (ctx.actualContentLength == 0) {
				debug("actualContentLength == 0, setting putContentLength = 16")
				ctx.putContentLength = 16
			}			
			retdat
		}

		override def |<|(data: Array[Byte]) = {
			//store the encryption key, but make sure to follow symlinks and recomputeFullPath to cover all cases
			setCrypto(followLink(ctx.recomputeFullPath(fullpath)),raw,IV)
			data
		}
		
	}
   
	//
 	class Decryption[T](implicit val ctx: RootContext[T])  extends MetaData[T] with PipeItem   with Serializable{
		//Initialize a Cipher
		private val cipher = Cipher getInstance "AES/CBC/PKCS5Padding"
		private var tempbuf : Array[Byte] = Array[Byte]()
		private var running_length = 0
		private var fullpath = ""
		
		override def ||() {
			
			fullpath = followLink(stripTrailingSlash("/" + ctx.user + ctx.verb.header("resource")))
			//Collections need not be decrypted (nor an be) but will be streamed back as HTML to the browser!
			if((! ctx.store.isCollection) && (exists(fullpath))) {
				//Get the zk path
				//Set up the decryption infra
				val keygen = KeyGenerator getInstance "AES"
				//Get key and intialization vector
			 	val crypt_data = getCrypto(followLink(fullpath))
				//And create the Cipher
	      		val raw = crypt_data _1
	      		val IV = crypt_data _2
				val ivspec = new IvParameterSpec(IV)
				val secretkeyspec = new SecretKeySpec(raw,"AES")
	      		debug("raw crypto key length = " + (raw length).toString)
	      		debug("raw iv spec length = " + (IV length).toString)
				cipher init(Cipher DECRYPT_MODE,secretkeyspec,ivspec)
			}
		}
		
		override def <|(data: Array[Byte]) = {
			
			//Collections need not be decrypted (nor an be) but will be streamed back as HTML to the browser!
			if((! ctx.store.isCollection) && (exists(fullpath))) {
				if(!(data==null) && !(tempbuf == null)) {
					//Get the length of the incoming data
					val datalen = data length;
	        		debug("Decryption, <|, data length = " + (data length).toString)
					running_length += (data length)
					debug("<| decryption running_length = " + (running_length toString))
					//Update the cipher
					val ret_dat = cipher update data
	        		//Guard against null values from the cipher
	        		ret_dat match {
	          			case a: Array[Byte] => {
	            			debug("<| , decrypted data length = " + (a length).toString)
	            			a
	          			}
	          			case null => { Array[Byte]()}
	        		}
				} else {Array[Byte]()}
			} else {data}
		}
		
		override def |<|(data: Array[Byte]) = {
			
			//Proflier plumbr showed this as "leak"
			tempbuf = null
			//Collections need not be decrypted (nor an be) but will be streamed back as HTML to the browser!
			if((! ctx.store.isCollection) && (exists(fullpath))) {
				if (!((data length) == 0)) { 
					debug("In update data != 0 clause, finalizing")
					debug("data length = " + (data length).toString)
	        		val ret_dat = cipher doFinal(data)
					debug("Cipher finalized for decryption")
	        		//Guard against null values from the cipher
	        		ret_dat match {
	          			case a: Array[Byte] => {
	            			debug("|<| , decrypted data length = " + (a length).toString)
	            			a
	          			}
	          			case null => { 
							debug("ret_dat == null")
							Array[Byte]()
						}
	        		}
	      		}
				else {
					debug("In update data == 0 clause, finalizing")
					//guard against exception with zero bytes
					var ret_dat = Array[Byte]()
					try {
	        			ret_dat = cipher doFinal()
					} catch { case _ => { Array[Byte]()}}
					debug("Cipher finalized for decryption")
	        		//Guard against null values from the cipher
	        		ret_dat match {
	          			case a: Array[Byte] => {
	            			debug("|<| , decrypted data length = " + (a length).toString)
	            			a
	          			}	
	          			case null => { 
							debug("ret_dat == null")
							Array[Byte]()
						}       			
					}      
	      		} 
			} else {data}
		}
	}
}