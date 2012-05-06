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
import net.vrijheid.clouddrive.providers._
import java.io.{InputStream,OutputStream,File,FileInputStream,FileOutputStream}
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.config.Config
import net.vrijheid.clouddrive.sharing._

package net.vrijheid.clouddrive.providers.filesystem {

	class FileSystemStore[T](key : String)(implicit ctx : RootContext[T]) extends Sharing[T] with Storage {
		
		//In ZooKeeper, our path is user + key
		private val user : String = "/" + ctx.user
		//private val fullkey = key startsWith user match {
		//	case true => followLink(stripTrailingSlash(key))
		//	case false => followLink(stripTrailingSlash(user + key))
		//}
		
		//Backpport from AWSFilesystem driver
		private val fullkey = key startsWith user match {
			case true => {
				debug("following link for actual user")
				followLink(stripTrailingSlash(key))
			}
			case false => {
				debug("following link for other user (sharing etc....)")
				if(sharedSource(key)) {
					debug("shared resource, following link")
					followLink(stripTrailingSlash(key))
				}
				else { 
					debug("following link, prepending user "+user+" first.")
					followLink(stripTrailingSlash(user + key))
				}
			}
		}
		
		
		debug("In FileSystemStore fullkey = " + fullkey)
		private val fs_prefix = Config("filesystem_prefix","/tmp/")
		private val user_files = fs_prefix +  user 
		(new File(user_files)).exists match {
			//Directory exists, do nothing
			case true => user_files
			case false =>  (new File(user_files)).mkdirs()
		}
		private var file : File = getOriginal(fullkey) match {
			case "" => new File(user_files + "/" + UUID)
			case s : String => new File(s)
		}
		var input : InputStream = _
		var output : OutputStream = _
		private val buffersize = 16384
		private var mode = 'undefined

		//We might need to clean up on onerwriting a file (i.e. uploading new content)
		//If we do upload, the value can be set to a closure that does just that
		//Default: do nothing
		//TBD: use this for versioning (i.e. then leave the original in place)
		private var cleanup_on_write : () => Unit = { () =>{()}}
		
		def open(how: Symbol) : Boolean = {

			
			how match {
				case 'read => {
					input = new FileInputStream(file)
					mode = 'read
					true
				}
				
				case 'write => {
					//Create the file if doesn't exist
					if (! (file exists)) {create}
					//It does exist, create a new file and make sure there is a cleanup_on_write routine
					else {
						val old_file = file
						file = new File(fs_prefix + UUID)
						create()
						cleanup_on_write = {() => {
							try {old_file delete} catch {case _ => {}}
							()}
						}
					}
					output = new FileOutputStream(file)
					mode = 'write
					true
				}
				
				case _ => {false}
			}
		}
		
		def close() {
			//Close input stream	
			mode match {		
			 	case 'read => { try {input close} catch {case e => {}}}
			 	case 'write => { try {output close} catch {case e => {}}}
				case _ => {}
			}
		}
		
		def read() : Array[Byte] = {
			
			var size = -1 
			val data = new Array[Byte](buffersize)
			
			if (!(null == input)) {
				size = input read(data,0,buffersize)
			}

			
			size match {
				case -1 => {Array[Byte]()}
				case _ => { data slice(0,size)}
			}			
		}
		
		def write(data : Array[Byte])  {
			if (!(null == output) && !(null == data)) {
				output write(data)				
			}
		}
		
		def transfer {
			//NOOP for local filesysten from WebDAV
			//Website uses transferLocalFile() below
		}
		
		def transferLocalFile(src: File,newuuid: String) {
			file = new File(user_files + "/" + newuuid)
			debug("In transferLocalFile, new path = "+file.getAbsolutePath)
			if(! src.renameTo(file)) {
				debug("Moving failed, now trying to copy")
				copyFile(src,file)
			}
		}
		
		def copy() = {
			val uuid = UUID
			val copied = new File(user_files+"/"+ uuid)
			copyFile(file,copied)
			(user_files+"/"+ uuid)
		}
		
		def getMetaData() : Map[String,String] = {
			
			val filesize = (file length) toString()
			val lastmodified = (file lastModified) toString()

			var data = Map[String,String]()
			//add metadata from ZK to the map
			data ++ getMetaData(fullkey)
		}
		
		def setMetaData(metadata : Map[String,String] ) {
			setMetaData(fullkey,metadata)
		}
		

		def exists() : Boolean  = {
			debug("exists for original: " + getOriginalName)
			//Are we dealing with a resource or a collection
			hasChildren match {
				//Collection
				case false => {exists(fullkey)}
				//resource
				case true => {(file exists) && (exists(fullkey))}
			}
			
		}
		
		def hasChildren() : Boolean = {hasChildren(fullkey)}
				
		def create() : Boolean = {
			
			//Test to see if it is a collection or a file as well
			//If it is a file, create it.
			ctx.verb match {
				case m : MKCOL => {
					createCollection(fullkey)
				}
				case _ => {
					createResource(fullkey)
					file createNewFile()
					setOriginal(fullkey,file getAbsolutePath)
				}
			}
			true

		}
		
		def deleteDirectly(uuid: String) {
			(new File(fs_prefix + uuid)).delete
		}
		           
		def delete() {
			debug("In FileSystemStore delete")
			debug("Exists: " + (exists toString))
			debug("Fullpath = " + (file getAbsolutePath))
			if (exists) {
				//TBD: make this recoverable
				file delete
			}
		}
		
		def isCollection() = {
			isCollection(fullkey)
		}
		
		def getOriginalName() = {
			file getAbsolutePath
		}
		
		
		//override def finalize() {close}
	}	
}