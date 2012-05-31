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
 
import java.security.MessageDigest
import java.util.{List => JList,_}
import java.text._
import java.io.{FileInputStream,FileOutputStream,File,InputStream,OutputStream,ByteArrayOutputStream,ByteArrayInputStream,ObjectOutputStream,BufferedOutputStream,IOException}
import java.util.zip._
import java.nio.channels._
import net.vrijheid.clouddrive.config._
import scala.actors.Actor
import java.io.{Serializable}

package net.vrijheid.clouddrive.utils { 

	//
	trait Debug  extends Serializable {
		
		private val level_s = Config.config.getOrElse("loglevel","debug")
		private val loglevel : Symbol = Symbol(level_s)


		def debug(msg : String) {
			probe('debug,msg)
		}

		def debug(value : AnyRef) {
			probe('debug,value)
		}
		def warn(msg : String) {
			probe('warn,msg)
		}

		def warn(value : AnyRef) {
			probe('warn,value)
		}
		def error(msg : String) {
			probe('error,msg)
		}

		def error(value : AnyRef) {
			probe('error,value)
		}

		def probe(msg : String) {
			probe('all,msg)			
		}

		def probe(what : Symbol, msg : String) {
			if ((what == loglevel) || (what == 'all )) {
				scala.Console println(msg)
			}
		}

		def probe(value : AnyRef) {
			probe('all,loglevel,value)			
		}
		
		def probe(what : Symbol, value : AnyRef) {
			if ((what == loglevel) || (what == 'all )) {scala.Console println((Actor.self.hashCode).toString + value)}
		}
		
		
	}	

	//
	trait Utils  extends Serializable  {
		
		def hopefully[T](f: => T): Option[T] = { 
			try {Some(f)} 
			catch { case _ => None}
		}
		
		def hopefully[T](handler: PartialFunction[Throwable, T], f: => T): Option[T] = {  
		    try {  
		      Some(f)  
		    } catch {  
		      case t if handler.isDefinedAt(t) => Some(handler(t))  
		      case _ => None 
		    }  
	  	}
		
		def intToByteArray(value: Int) = {
			Array[Byte](
				(value >>> 24).asInstanceOf[Byte],
				(value >>> 16).asInstanceOf[Byte],
				(value >>> 8).asInstanceOf[Byte],
				(value).asInstanceOf[Byte] 
				)
		}
		
		def byteArrayToInt(b: Array[Byte]) = {
			(b(0) << 24) + ((b(1) & 0xFF) << 16) + ((b(2) & 0xFF) << 8) + (b(3) & 0xFF)
		}
		
		//Some sbyte/object compression, handy for compact serialization
		def compress(bytes: Array[Byte]) = {

			val ba = new ByteArrayOutputStream()
			val gz = new GZIPOutputStream(ba)
			gz write bytes;
			gz close;
			val compressed = ba toByteArray()
			ba close;
			compressed
		}
		
		def decompress(bytes: Array[Byte]) = {
			val bi = new ByteArrayInputStream(bytes)
			val gz = new GZIPInputStream(bi)
			var buf = new Array[Byte](16384)
			var decompressed = Array[Byte]()
			
			var size = gz.read(buf,0,buf.length)
			while (size > -1) {
				decompressed = decompressed ++ buf.slice(0,size)
				size = gz.read(buf,0,buf.length)
			}
			gz close;
			decompressed
		}
		
		def compress(obj: java.io.Serializable) = {

			val ba = new ByteArrayOutputStream()
			val gz = new GZIPOutputStream(ba)
			val obstr = new ObjectOutputStream(gz)
			obstr writeObject(obj);
			obstr flush;obstr close;
			gz close;
			val compressed = ba toByteArray()
			ba close;
			compressed
		}
		
		def previousUserMonthYear(user: String): String = {
			val cal = Calendar getInstance;
			cal.roll(Calendar.MONTH,false)
			user + (new SimpleDateFormat("-MMM-yyyy",Locale.US)).format(cal getTime)
		}

		def nextUserMonthYear(user: String): String = {
			val cal = Calendar getInstance;
			cal.roll(Calendar.MONTH,true)
			user + (new SimpleDateFormat("-MMM-yyyy",Locale.US)).format(cal getTime)
		}		
		
		//These functions make bookkeeeping with time easier (especially using voldemort)
		def userMonthYear(user: String) = {
			user + (new SimpleDateFormat("-MMM-yyyy",Locale.US)).format(new Date())
		}
		
		def userYear(user: String) = {
			user + (new SimpleDateFormat("-yyyy",Locale.US)).format(new Date())
		}

		def daysOfThisMonth() = {
			(Calendar.getInstance).getMaximum(Calendar.DAY_OF_MONTH)
		}
		
		def daysOfPreviousMonth() = {
			//Gte a calendar, roll one month back, then fetch how many days
			val cal = Calendar.getInstance
			cal.roll(Calendar.MONTH,false)
			cal.getMaximum(Calendar.DAY_OF_MONTH)
		}
		
		def daysOfNextMonth() = {
			//Gte a calendar, roll one month forward, then fetch how many days
			val cal = Calendar.getInstance
			cal.roll(Calendar.MONTH,true)
			cal.getMaximum(Calendar.DAY_OF_MONTH)	
		}
		
		def currentMonthHour(): Int = {
			val cal = Calendar.getInstance;
			val dof = -1 + cal.get(Calendar.DAY_OF_MONTH)
			val hour = cal.get(Calendar.HOUR_OF_DAY)
			hour + (24 * dof) 
		}

		def hoursOfThisMonth() = {
			24 * daysOfThisMonth
		}

		def hoursOfPreviousMonth() = {
			24 * daysOfPreviousMonth
		}

		def hoursOfNextMonth() = {
			24 * daysOfNextMonth
		}

		//Of course we want this
		def UUID() = { (java.util.UUID.randomUUID()).toString}

		def idate(now: Date) = {
			(new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zZ", Locale.US)).format(now)
		}
		
		def idateNow() = {
		 idate(new Date())
		}
		
		def isoDate(now : Date) = {
			(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")).format(now)
		}
		
		def isoDateNow() = {
			isoDate(new Date())
		}

		//Hex encoding/decoding
		def hex2Bytes(hex: String): Array[Byte] = {
		  (for{i <- 0 to hex.length - 1 by 2 if i > 0 || !hex.startsWith("0x")}
		  yield hex.substring(i, i + 2)).map(Integer.parseInt(_, 16).toByte).toArray
		}

		def bytes2Hex(bytes: Array[Byte]) = bytes.map(b => "%02X".format(b)).mkString
		
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

		def sha1(data: String): String = {
		  sha1(data.getBytes("iso-8859-1"))
		}

		def sha1(data: Array[Byte]): String = {
		  val sha1 = MessageDigest.getInstance("SHA1")
		  sha1 update data
		  bytes2Hex(sha1 digest ())
		}

		//....and these haven't been done as per lack of need
		def computeHMACMD5(data: String): String = {
		  computeHMACMD5(data.getBytes("iso-8859-1"))
		}

		def computeHMACMD5(data: Array[Byte]): String = {
		  computeHMACMD5(data)
		}

		def computeHMACSHA1(data: String): String = {
		  computeHMACSHA1(data.getBytes("iso-8859-1"))
		}

		def computeHMACSHA1(data: Array[Byte]): String = {computeHMACSHA1(data)}

		//Handy for HTTP Digest authN
		def generateNonce() = {
			val r = new Random()
			var bnonce = new Array[Byte](16)
			r.nextBytes(bnonce)
			(bytes2Hex(bnonce)) toLowerCase
		}	

		//Remove the " from a string
		def stripQuotes(text : String) = {
			text replaceAll("\"","")
		}
		
		//Turn /something into something
		def stripTrailingSlash(s : String) = {
			var t = s 
			if (t endsWith "/") {t = t dropRight(1)}
			t
		}
		
		def stripLeadingSlash(s: String) = {
			var t = s 
			if (t startsWith "/") {t = t drop(1)}
			t			
		}
		//We use a tail recursive function for this
		//@tailrec
		private def allSubDirFiles(result : List[File],workload : List[File]) : List[File] = {
			//Go to the the first dir
			val currentdir = workload(0)
			//Get its contents, to list, and filer out non directories
			val contents = (currentdir listFiles) toList;
			val dir_contents = contents filter { f => {f isDirectory}}
			val file_contents = contents filter { f => {f isFile}}
			val workload_n = (workload tail) ++ dir_contents 
			workload_n match {
				//...Workload empty! 
				case List() => {result ++ file_contents}
				//tail call
				case _ => allSubDirFiles(file_contents ++ result,workload_n)
			}
		}
		
		//Again.... why isn't this "just there"  in Java?
		def allSubDirFiles(path : String) : List[File] = {
			allSubDirFiles(List[File](),List(new File(path))) 
		}

		//We use a tail recursive function for this
		//@tailrec
		private def allSubDirs(result : List[File],workload : List[File]) : List[File] = {
			//Go to the the first dir
			val currentdir = workload(0)
			//Get its contents, to list, and filer out non directories
			var contents = (currentdir listFiles) toList;
			//var contents = contents_seq toList;
			contents = contents filter { f => {f isDirectory}}
			val workload_n = (workload tail) ++ contents 
			workload_n match {
			//...Workload empty! 
			case List() => {result}
			//tail call
			case _ => allSubDirs(currentdir :: result,workload_n)
			}
		}
		//And again....
		def allSubDirs(path : String) : List[File] = {
			allSubDirs(List[File](),List(new File(path))) 
		}
		
		//Better finds....		
		def findIndexOf(s: String,c: Char) : Int = {
			val predicate = { (k: Char) => { k == c}}
			s.indexWhere(predicate,0);
		}
		
		def findIndexOf(s: String,key: String): Int = {
			if ((key length) == 1) {findIndexOf(s,key charAt 0)}
			else {-1}
		}	
			
		def findIndexOfReverse(s: String,c: Char) : Int = {
			val predicate = { (k: Char) => { k == c}}
			//FInd the index or -1 on the reversed String
			val rindex = (s reverse).indexWhere(predicate,0);
			rindex match {
				case -1 => {-1}
				case _ => { (s length) - rindex}
			}
		}
		
		def findIndexOfReverse(s: String,key: String): Int = {
			if ((key length) == 1) {findIndexOfReverse(s,key charAt 0)}
			else {-1}
		}	
			
		/* We do this too many times inline, returns the filename from a full path */	
		def fileNameFromPath(path: String): String = {
			path.slice(findIndexOfReverse(path,"/"),path.length)
		}
		
		def pathFromFullPath(path: String): String = {
			if (path.length > 0) { path.slice(0,(findIndexOfReverse(path,"/") - 1))}
			else path
		}
				
		//OK
		def dropUserIndexFromPath(path: String,user: String): String  = {
			if (path startsWith "/") {
				val cleaneduser = path drop(1)
				cleaneduser.drop(findIndexOf(cleaneduser,"/"))
			}
			else { path.drop(findIndexOf(path,"/"))}
		}

		//OK
		def getTopLevelPath(path: String): String = {
			if (path startsWith ("/")) {
				val cleanedpath = path drop(1)
				cleanedpath slice(0,findIndexOf(cleanedpath,"/"))
			}
			else {
				path slice (0,findIndexOf(path,"/"))
			}
		}
		
		//OK		
		def dropTopLevelPath(path: String): String = {
			if (path startsWith "/") {
				val cleanedpath = path drop(1)
				cleanedpath.drop(findIndexOf(cleanedpath,"/"))
			}
			else { path.drop(findIndexOf(path,"/"))}			
		}
		
		
		//Extract the usrname from a path
		def getUserNameFromPath(path: String) ={
			val localpath = path drop 1
			if (localpath contains "/") {
				localpath.substring(0,findIndexOf(localpath,"/"))
			}
			else localpath
		}
			
		//We don't want exceptions on substrings, sometimes. Really.
		def safeSubstring(s: String,i: Int) = {
			if ((s length) >= i) {
				s substring i
			} else {""}
		}
		
		//(and again)
		def copyFile(in: File,out: File) {
			
			val inChannel = (new FileInputStream(in)).getChannel()
	        val outChannel = (new FileOutputStream(out)).getChannel()
	        try {
	            inChannel.transferTo(0, inChannel.size(),outChannel)
	        } 
	        catch  {
	            case e : IOException => {throw e}
				case _ => {}
	        }
	        finally {
	            if (inChannel != null) {inChannel.close()}
	            if (outChannel != null) {outChannel.close()}
	        }
		}
		
		//Sone arbitrarily chosed encoding weirdness for WebDAV
		def encodeNamespace(ns: String,key:String) = {
			(ns replaceAll("/","!")) + "*" + key
		}
		
		def decodeNamespace(key: String) = {
			val nskey = key.replaceAll("!","/")
			val index = findIndexOfReverse(nskey,"*" charAt(0))
			val namespace = (nskey slice(0,(index -1)))
			(namespace,nskey slice(index,nskey length))
		}
		
		def extractNamespace(ns: String) = {
			val lindex = findIndexOf(ns,":" charAt 0)
			val rindex = findIndexOf(ns,"=" charAt 0)
			ns slice (lindex + 1,rindex)
		}
		
		def extractNamespaceURI(ns: String) = {
			val rindex = findIndexOf(ns,"=" charAt 0)
			ns slice (rindex + 1,(ns length))
		}
		
		def in2out(in: InputStream,out: OutputStream,filter: {def filter(data: Array[Byte]): Array[Byte];def wrapUp(data: Array[Byte]): Array[Byte]},length: Long,bufsize: Int = 16384) {
		
			//Some bookkeeping variables
			var remaining : Long = length
			var buf = new Array[Byte](bufsize)
			val buf_out = new BufferedOutputStream(out)
			var offset = 0
		
			//The copy loop
			do {
				val numread = in.read(buf,0,buf.length)
				if (numread > 0) {
					val filtered_data = filter.filter(buf.slice(0,numread))
					buf_out write(filtered_data)
					remaining -= numread
				}
				//offset = (
				//debug("in2out, offset="+offset)
			} while( remaining > 0)
			
			//If we have data read that still needs to be pushed through the filter, here's our final chance
			//if(offset > 0) {
				//Call wrapup on our final piece of data
			//	val wrap = filter.wrapUp(buf.slice(0,offset))
				//And write whatever we have....
			//	buf_out write(wrap)
			//}
			//All data has already been written, so wrap up with an empty Array
			val wrap = filter.wrapUp(Array[Byte]())
			buf_out write wrap;
			//flush our output
			buf_out flush;
		}
		
		def in2out(in: InputStream,out: OutputStream,filterr: {def filter(data: Array[Byte]): Array[Byte]},length: Long) {
			val wrapper = new {
				def filter(data: Array[Byte]) = {filterr.filter(data)}
				def wrapUp(data: Array[Byte]) = filter(data)
			}
			in2out(in,out,wrapper,length)
		}
		
		def in2out(in: InputStream,out: OutputStream,length: Long) {
			val wrapper = new {def filter(data: Array[Byte]) = {data}}
			in2out(in,out,wrapper,length)
		}
				
   	}


	//
	//Same for this trait
	trait Treatise extends Utils with Debug with Base64  with Serializable {}
	
	//
	//Extending this class will give you all Utils "magically"
	class Handy() extends Treatise  with Serializable
}