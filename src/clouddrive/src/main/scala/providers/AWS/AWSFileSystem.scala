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
import net.vrijheid.clouddrive.httpsupport._
import net.vrijheid.clouddrive.providers.{MetaData}
import net.vrijheid.clouddrive.sharing.{Sharing}
import java.io.{InputStream,OutputStream,File,FileOutputStream}
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.config.Config
import com.amazonaws.services.s3._
import com.amazonaws.auth.{BasicAWSCredentials}
import com.amazonaws.services.s3.model.{S3Object}

package net.vrijheid.clouddrive.providers.aws {
	
	class AWSFileSystem[T](key : String)(implicit ctx : RootContext[T]) extends Sharing[T] with Storage {
		
		//Code CC for sharing:
		//Both PUT and GET go wrong. Sometimes after sharing the source disappears.
		
		private val user : String = "/" + ctx.user
		debug("In AWSFileSystem, key startsWith user = " + (key startsWith user))
		debug ("key was "+ key)
		private val fullkey = key startsWith user match {
			case true => {
				debug("following link for actual user")
				followLink(stripTrailingSlash(key))
			}
			case false => {
				debug("following link for other user (sharing etc....)")
				if(sharedResource(key)) {
					debug("shared resource, following link")
					followLink(stripTrailingSlash(key))
				}
				else { 
					debug("following link, prepending user "+user+" first.")
					followLink(stripTrailingSlash(user + key))
				}
			}
		}
		
		//private val fullkey = followLink(key)
		
		debug("In AWSFileSystem fullkey = " + fullkey)

		private var guid = (exists(fullkey) match {
			case true => { getOriginal(fullkey) }
			case false => {UUID}
		})

		debug("In AWSFileSystem, guid ="+guid)
		var input : InputStream = _
		var output : OutputStream = _
		private val buffersize = 16384
		private var mode = 'undefined		
		private var cleanup_on_write : () => Unit = { () =>{(debug("empty cleanup_on_write"))}}
		//create S3 connection, get bucket
		private val s3conn = new AmazonS3Client(new BasicAWSCredentials(Config("awsid"),Config("secretkey")))
		private val bucket = Config("bucket")
		private var item : S3Object = _
		private var old_guid = ""
    	private var cumulation = 0
		private var file : File = _
		private var transferable = false


		def open(how: Symbol) : Boolean = {

			how match {
				case 'read => {
					//Open the item and set the inputstream internally as well
					item = s3conn getObject(bucket,guid)
					input = item getObjectContent;
					mode = 'read
					true
				}

				case 'write => {
					//Create the file if doesn't exist
					exists(fullkey) match {
						//Exists, delete it in cleanup_on_write
						//NOTE: we can add versioning here (i.e. NO delete in cleanup_on_write)
						case true => {
							debug("In AWSFileSystem, open 'write, key exists")
							old_guid = guid
							guid = UUID
							//DO this immediately, because on close might be scheduled a bit later then the next request
							setOriginal(fullkey,guid)
							debug("Original (re?)set")
							debug( "old, new guid: " + old_guid + " , " + guid)
							cleanup_on_write = {() => {
									try { 
										debug("AWSFileSystem, cleanup_on_write (non-chunked)")
										//If we want to add it: Here we need to add the versioning
										//Reset the original to the (new) guid
										//setOriginal(fullkey,guid)
										debug("***********************************************************")
										debug("orginal updated in metadata to " + guid)
										debug("was" + old_guid)
										debug("new original is" + getOriginal(fullkey))
										debug("***********************************************************")
										s3conn deleteObject(bucket,old_guid);
										debug("Old item deleted from S3")
										//Allow a catch all on error, tha simply means we don't delete data
									} catch {case _ => {}}
								}
							}
							create()
							setOriginal(fullkey,guid)
							debug("all created")
						}
						
						case false => {
							debug("In AWSFileSystem, open 'write, key does not exist")
							cleanup_on_write = {() => {
									try {
										debug("cleanup_on_write, no chunked encoding, setting original")
										setOriginal(fullkey,guid)
									} catch { case _ => {"Exception..."}}
								}
							}
							create()
						}
					}
					//Set the mode to write
					mode = 'write
					true
				}

				case _ => {false}
			}
		}

		def close() {
			debug("In AWSFileSystem, close()")

			//Close input stream	
			mode match {
						
			 	case 'read => { try {input close} catch {case e => {}}}
			 	case 'write => { 
					try {
						debug("Original (re?)set")
						debug("Closing ")
						try {
							output flush;
							output close;
							debug("output stream closed")
						} catch { case _ => {debug("excepton while closing output stream")}} 
						cleanup_on_write()
						debug("cleanup_on_write executed in AWSFileSystem close()")
					} catch {case e => { e toString}}}
				case _ => {}
			}
			debug("AWSFileSystem closed.")
		}

		def read() : Array[Byte] = {

			var size = -1 
			val data = new Array[Byte](buffersize)

      		debug("input = " + input)
			if (!(null == input)) {
        		debug("input not null. trying to read data from S3 stream")
				size = input read(data,0,buffersize)
				//size = input read(data)
			}

      		debug("size read = " + (size toString))
			size match {
				case -1 => {
          			debug("AWSfilesystem size read = -1")
          			debug("---cumulation = " + cumulation)
          			Array[Byte]()
        		}
				case _ => { 
          			cumulation += size
          			debug("***cumulation = " + cumulation)
          			data slice(0,size)
        		}
			}			
		}

		def write(data : Array[Byte])  {
			debug ("In AWSFileSystem write")
			if (!(null == output) && !(null == data)) {
				debug("Writing " + ((data length) toString) + " bytes of data")
				output write(data)				
			}
		}

		def transfer {
			if (transferable) {
				try {
					output close;
					debug("File closed")
					s3conn putObject(bucket,guid,file)
					debug("File uploaded")
					file delete;

				} catch { case e => { debug(e toString)}}
			}
			
		}
		
		def transferLocalFile(src: File,newuuid: String) {
			try {
				s3conn putObject(bucket,newuuid,src)
				src delete;
			} catch { case e => {debug(e toString)}}
		}

		def copy() = {
			val newuuid = UUID;
			s3conn copyObject(bucket,guid,bucket,newuuid)
			newuuid;
		}

		def getMetaData() : Map[String,String] = {
			//Return metadata from metadata store
			getMetaData(fullkey)
		}

		def setMetaData(metadata : Map[String,String] ) {
			setMetaData(fullkey,metadata)
		}

		def exists() : Boolean  = {
			exists(fullkey)
		}

		def hasChildren() : Boolean = {hasChildren(fullkey)}

		def create() : Boolean = {

			debug("In AWSFileSystem, create, key = " + fullkey)
			//Create entry in Metadata store
			//Test to see if it is a collection or a file as well
			//If it is a file, create it.
			ctx.verb match {
				//Collection... create already in ZooKeeper
				case m : MKCOL => {createCollection(fullkey)}
				//File/resource: just create the pipe to S3. Metadata store gets handled by our calling Verb upon completion
				case _ => {
					createResource(fullkey)
					var encoding = ((ctx.verb.header.getOrElse("Transfer-Encoding","not chunked")).trim) toLowerCase;
					//Creating a stream for an object....
					debug(fullkey + " created.")
					//When using http 1.1 chunked encoding we need to buffer locally before sending to S3
					encoding match {
						case "chunked" => {
							debug("S3FileSystem, chunked encoding. Buffer via file")
							file = new File("/tmp/" + guid)
							file createNewFile;
							output = new FileOutputStream(file)
							debug("File " + file.getAbsolutePath + " created.")
							transferable = true
						}
						//Not chunked, pipe directly
						case "not chunked" => {
							val content_length = ctx.putContentLength 
							debug("AWSFileSystem, not chunked encoding. Buffer via file")
							file = new File("/tmp/" + guid)
							file createNewFile;
							output = new FileOutputStream(file)
							debug("File " + file.getAbsolutePath + " created.")
							transferable = true							
						}
					}
				}
			}
			true

		}

		def deleteDirectly(uuid: String) {
			s3conn deleteObject(bucket,uuid)
		}

		def delete() {
			debug("In AWSFileSystem delete")
			debug("S3 name, guid: " + getOriginalName + " , " + guid)
			debug("Exists: " + (exists toString))
			
			if (exists) {
				//TBD: make this recoverable
				//Only delete if this is NOT a collection
				//Otherwise the guid is empty...
				if (! isCollection(fullkey)) { s3conn deleteObject(bucket,guid)}
			}
		}

		def isCollection() = {
			isCollection(fullkey)
		}

		def getOriginalName() = {
			//For S3, a UUID stored in /original under the key in the Metadata store, aka the guid
			guid
		}
		
	}
	
	
}