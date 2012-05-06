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
import java.io.{FilterOutputStream,OutputStream,InputStream,FilterInputStream,EOFException,IOException}
import java.util.zip.{Deflater,Inflater,DataFormatException}
import scala.math._

package net.vrijheid.clouddrive.utils {
	
	class CompressedBlockOutputStream(os: OutputStream,size: Int = 16384) extends FilterOutputStream(os) {
	    /**
	     * Buffer for input data
	     */
	    private var inBuf: Array[Byte]  = new Array[Byte](size)

	    /**
	     * Buffer for compressed data to be written
	     */
	    private var outBuf: Array[Byte]  = new Array[Byte](size + 64)

	    /**
	     * Number of bytes in the buffer
	     */
	    private var len = 0

	    /**
	     * Deflater for compressing data
	     */
	    private val deflater: Deflater = new Deflater(Deflater.DEFAULT_COMPRESSION)
		deflater.setStrategy(Deflater.DEFAULT_STRATEGY)


	    def compressAndSend() {
	        if (len > 0) {
	            deflater.setInput(inBuf, 0, len);
	            deflater.finish();
	            val size = deflater.deflate(outBuf);

	            // Write the size of the compressed data, followed
	            // by the size of the uncompressed data
	            out.write((size >> 24) & 0xFF);
	            out.write((size >> 16) & 0xFF);
	            out.write((size >>  8) & 0xFF);
	            out.write((size >>  0) & 0xFF);

	            out.write((len >> 24) & 0xFF);
	            out.write((len >> 16) & 0xFF);
	            out.write((len >>  8) & 0xFF);
	            out.write((len >>  0) & 0xFF);

	            out.write(outBuf, 0, size);
	            out.flush();

	            len = 0;
	            deflater.reset();
	        }
	    }

	    override def write(b: Int)  {
			len += 1
	        inBuf(len) = b.toByte;
	        if (len == inBuf.length) {
	            compressAndSend();
	        }
	    }

	    override def write(b: Array[Byte],pboff: Int,pblen: Int) {
			var boff = pboff
			var blen = pblen
	        while ((len + blen) > inBuf.length) {
	            val toCopy = inBuf.length - len
				inBuf = inBuf.slice(0,len) ++ b slice(boff,boff + toCopy)
	            len += toCopy;
	            compressAndSend();
	            boff += toCopy;
	            blen -= toCopy;
	        }
			inBuf = inBuf.slice(0,len)  ++ b.slice(boff,boff + blen)
	        len += blen;
	    }

	    override def flush() {
	        compressAndSend()
	        out.flush()
	    }

	    override def close() {
	        compressAndSend()
	        out.close()
	    }
	}
	
	class CompressedBlockInputStream(is: InputStream) extends FilterInputStream(is) {
	    /**
	     * Buffer of compressed data read from the stream
	     */
	    private var inBuf: Array[Byte]= _

	    /**
	     * Length of data in the input data
	     */
	    private var inLength = 0

	    /**
	     * Buffer of uncompressed data
	     */
	    private var outBuf: Array[Byte] = _

	    /**
	     * Offset and length of uncompressed data
	     */
	    private var outOffs = 0
	    private var outLength = 0

	    /**
	     * Inflater for decompressing
	     */
	    private val inflater = new Inflater



	    private def readAndDecompress() {
	        // Read the length of the compressed block
	        var ch1 = in.read();
	        var ch2 = in.read();
	        var ch3 = in.read();
	        var ch4 = in.read();
	        if ((ch1 | ch2 | ch3 | ch4) < 0) throw new EOFException();
	        inLength = ((ch1 << 24) + (ch2 << 16) + (ch3 << 8) + (ch4 << 0));

	        ch1 = in.read();
	        ch2 = in.read();
	        ch3 = in.read();
	        ch4 = in.read();
	        if ((ch1 | ch2 | ch3 | ch4) < 0)throw new EOFException();
	        outLength = ((ch1 << 24) + (ch2 << 16) + (ch3 << 8) + (ch4 << 0));

	        // Make sure we've got enough space to read the block
	        if ((inBuf == null) || (inLength > inBuf.length)) {
	            inBuf = new Array[Byte](inLength)
	        }

	        if ((outBuf == null) || (outLength > outBuf.length)) {
	            outBuf = new Array[Byte](outLength)
	        }

	        // Read until we're got the entire compressed buffer.
	        // read(...) will not necessarily block until all
	        // requested data has been read, so we loop until
	        // we're done.
	        var inOffs = 0;
	        while (inOffs < inLength) {
	            val inCount = in.read(inBuf, inOffs, inLength - inOffs);
	            if (inCount == -1) {
	                throw new EOFException();
	            }
	            inOffs += inCount;
	        }

	        inflater.setInput(inBuf, 0, inLength);
	        try {
	            inflater.inflate(outBuf);
	        }
	        catch {
				case dfe: DataFormatException => {
	            throw new IOException(
	                "Data format exception - " +
	                dfe.getMessage())
				}
	        }

	        // Reset the inflator so we can re-use it for the
	        // next block
	        inflater.reset()
	        outOffs = 0
	    }

	    override def read(): Int = {
	        if (outOffs >= outLength) {
	            try {
	                readAndDecompress();
	            }
	            catch {
					case eof: EOFException => {
	                 	-1
					}
	            }
	        }
			
			outOffs += 1
	        (outBuf(outOffs) & 0xff)
	    }

	    override def read(b: Array[Byte],off: Int,len: Int):Int = {
		
			//This function implements a simple Array copy, reading from the stream
			//Decompress is possible, otherwise keep what we have until more data arrives
	        var count = 0;
	        while (count < len) {
	            if (outOffs >= outLength) {
	                try {
	                    // If we've read at least one decompressed
	                    // byte and further decompression would
	                    // require blocking, return the count.
	                    if ((count > 0) && (in.available() == 0)) { count}
	                    else
	                        readAndDecompress()
	                }
	                catch {
						case eof: EOFException => {
	                    	if (count == 0)	count = -1;
	                    	count
						}
	                }
	            }

	            val toCopy = min(outLength - outOffs, len - count);
				val src = outBuf slice (outOffs,outOffs + toCopy)
				src copyToArray (b,off + count,toCopy)
	            outOffs += toCopy;
	            count += toCopy;
	        }
	        count
	    }

	    override def available(): Int = {
	        // This isn't precise, but should be an adequate
	        // lower bound on the actual amount of available data
	        (outLength - outOffs) + in.available()
	    }
	}	
}