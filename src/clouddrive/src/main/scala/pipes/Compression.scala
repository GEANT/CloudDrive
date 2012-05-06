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


import java.util.zip.{Deflater,Inflater,DataFormatException}
import net.vrijheid.clouddrive.pipes._
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.providers.{MetaData}
import net.vrijheid.clouddrive.config._

package net.vrijheid.clouddrive.pipes {
	
	class Compressor[T](implicit ctx: RootContext[T]) extends PipeItem with Treatise {
		
		private val buffersize = (512 * 1024)
	    //Buffer for input data
	    private var inbuf: Array[Byte]  =  Array[Byte]()
		
		override def ||() {
			if (ctx.userConfig("compression","aes") != "aes") {
				//We need to reset te putContentLength to zero, because we'll be adding the real block sizes dynamically to it.
				ctx.putContentLength  = 0				
			}
		}
		
		private def modPutContentLength(put: Int) {
			ctx.putContentLength += put
		}
				
	    override def >|(data: Array[Byte])  = {
		
			val len = data.length
			inbuf = inbuf ++ data
			
			if ((len + inbuf.length)  > buffersize) {
				debug("In >|, compressing")
				var outbuf = compress(inbuf)
				outbuf = (intToByteArray(outbuf.length)) ++ outbuf;
				inbuf = Array[Byte]()
				//We have to modify the putContentLength, as the encryption won't do that for us
				if (ctx.userConfig("compression","gzip") != "gzip") {
					modPutContentLength(outbuf.length)
				}
				outbuf
			}
			else {Array[Byte]()}
	    }

	    override def |>|(data: Array[Byte])  = {
			val len = data.length
			if ((inbuf.length + len) > 0) {
				debug("In |>|, compressing")
				inbuf = inbuf ++ data
				var outbuf = compress(inbuf)
				debug("compressed buffer length = " + outbuf.length)
				outbuf = (intToByteArray(outbuf.length)) ++ outbuf
				debug("length added, outbuf length = " + outbuf.length)
				//We have to modify the putContentLength, as the encryption won't do that for us
				if (ctx.userConfig("compression","gzip") != "gzip") {
					modPutContentLength(outbuf.length)
				}
				outbuf
			}
			else {
				//We have to modify the putContentLength, as the encryption won't do that for us
				if (ctx.userConfig("compression","gzip") != "gzip") {
					modPutContentLength(data.length)
				}
				data
			}
	    }
	}
	
	class Decompressor[T](implicit ctx: RootContext[T]) extends PipeItem with Treatise {
		
	    // Buffer of compressed data read from the stream
	    private var inbuf: Array[Byte]= Array[Byte]()

	    // Buffer of uncompressed data
	    private var outbuf: Array[Byte] = Array[Byte]()
	
		private def decompressBlocks() {
			debug("In decompressBlocks")
			//Get the size of the nextinput block
			var bsize = inbuf.slice(0,4)
			var size = -1
			if (bsize.length == 4) {size = byteArrayToInt(bsize)}
			debug("Next block size = " + size)
			//If it's not -1 (no size yet ) and we have more in the inbuf than the size (correcting for the 4 bytes containing the length)
			//... decompress
			while ((size > -1 ) & (size <= (inbuf.length - 4))) {
				debug("local size = "+size)
				//Fill the output buffer
				outbuf = outbuf ++ decompress(inbuf.slice(4,size + 4));
				debug ("updated output buffer")
				//Move the input buffer forward
				inbuf = inbuf.slice(size + 4, inbuf.length)
				debug("moved input buffer forward, getting next size")
				//Get the size
				bsize = inbuf.slice(0,4)
				size = -1
				if (bsize.length == 4) {size = byteArrayToInt(bsize)}			
			}
		}


	    override def <| (data: Array[Byte]) = {
			
			debug("in <| , Decompressor")
			inbuf = inbuf ++ data
			debug("inbuf length = " + inbuf.length)
			decompressBlocks;
			if (outbuf.length > 0) { 
				debug("outbuf.length > 0")
				val retdata = outbuf
				outbuf = Array[Byte]()
				retdata
			} 
			else {
				debug("Nothing in outbuf")
				Array[Byte]()
			}
		}
		
	    override def |<|(data: Array[Byte]) =  { 
			debug("in |<| , Decompressor")
			<|(data) 
		}
			
	}	
}