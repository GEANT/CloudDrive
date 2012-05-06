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
import net.vrijheid.clouddrive._
import net.vrijheid.clouddrive.httpsupport._
import java.net._
import net.vrijheid.clouddrive._
import scala.actors._
import java.util.concurrent._

package net.vrijheid.clouddrive {
	//Our main Server class, takes as a parameter a port
	class MainServer(port: Int) {
	
		private val this.port = port
	
		def start() {
			//This should allow us to keep accepting connections if there is a lot to do
			(Thread.currentThread()).setPriority(Thread.MAX_PRIORITY)
			//val memcleaner = new MemCleanser()
			//memcleaner.setPriority(Thread.MAX_PRIORITY)
			//memcleaner.start;
			//Start a server socket
			val main_listener = new ServerSocket(this.port,1024)
			//We might make this configurable to prevent resource exhaustion
			//Need to test GC and JVM behavior first
			//val pool = Executors newFixedThreadPool(128);
			//val pool = Executors newCachedThreadPool();
			//Loop forever
			while (true) {
				//Accept a client
				//The try /catch is just a guard agains e.g. SSL exceptions
				try {
					val client = main_listener accept;
					client setSoTimeout(5000)
					//Start the Thread to handle it
					//We use threads in stead of nio, as we expect to be CPU intensive
					//i.e. on the fly piping to S3 with encryption
					//pool execute(new WebDavHandler(client))
					//The "no pool model"
					(new Thread(new WebDavHandler(client))).start()
				} catch {case _ => {}}
			} 
		
		}
	
	}

	class MemCleanser extends Thread {
	
		override def run() {
			while(true) {
				//FOrever, every 30 seconds
				//FIX_CC make this configurable, put it at 5 seconds now
				Thread sleep(30000);
				System runFinalization;
				System gc;
			}
		}
	
	}

	object MainServerStart  {
	
		import net.vrijheid.clouddrive.config._
	
		def main(args : Array[String]) = {
		
			//Load config from command line parameter 
			if ((args length)  == 1) {Config.loadConfig(args(0))}
			//else. default
			else {Config.loadConfig("/etc/rightfabric/config.txt")} 
			val port = Config("port","8080") toInt;
			//Optional ZooKeeper config, or MySQL etc. initialization		
			new MainServer(port) start 
		}
	
	}
}
