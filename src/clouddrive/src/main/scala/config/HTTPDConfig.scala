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
import java.util.{Properties}
import scala.collection.{JavaConversions => JC}
import java.io.{File,FileInputStream}
import scala.actors.scheduler._
import java.util.concurrent._
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.utils._
import java.io.{Serializable}

package net.vrijheid.clouddrive.config {
	
	//
 	class Config(values: Map[String,String])  extends Serializable {
		
		private var config = values
		
		//Some handy and obvious methods
		def put(key: String,value: String) { config = Map(key -> value) ++ config}
		def delete(key: String) { config = config - key}
		def update(key: String,newvalue: String) {
			delete(key)
			put(key,newvalue)
		}
		//Get it from this config, and on failure, try the global config
		def get(key: String): String = {
			
			//Get the value 
			config contains key  match {
				//If empty, lookup globally
				case false => { Config(key)}
				//Success, get it, again (in JITs we trust)
				case true => {config.get(key).get}
			}
		}
		//Get it from this config, and on failure, try the global config
		def get(key: String,default: String): String = {
			
			//Get the value
			config contains key  match {
				//Guard against the deault, then try global Config as well
				case false => { Config(key,default)}
				//"Just get it again"
				case true => {config.get(key).get}
			}
		}
		
		def apply(key: String,default: String): String = {get(key,default)}
		def apply(key: String): String = {get(key)}
		
	}
	
	
	object Config  {

		var config : Map[String,String] = Map {"" -> ""}
		
		def userConfig[T](implicit ctx: RootContext[T]) : Config = {
			
			val user = ctx.user
			
			Config("authnMethod","voldemort") match {
				
				//Legacy
				/**case s: String if s == "zookeeper" => {
					//Load userland config parameters from ZooKeeper
					val zk = ctx.zk
					val path = Config("auth_prefix","/auth") + "/" + user
					val children = zk getChildren path
					var config = Map[String,String]()
					children foreach {
						(child) =>
						{
							val data = zk getData(path + "/" + child)
							data match {
								//Append config value when not empty
								case "" => {}
								case _ : String => {
									config = config ++ Map(child -> data)
								}
							}
						}
					}
					new Config(config)
				}*/
				
				case s: String if s == "voldemort" => {
					val vmauthn = VMTalk getAuthnClient();
					//return the map from voldemort minus the realm and ha1 keys
					val config = (vmauthn getValue_?(user)) match {
						case None => Map[String,String]()
						case Some(map) => map -- List("realm","ha1")
					}
					//vmauthn.factory.close;
					new Config(config)
				}
				
			 	case s: String if s == "static" => { new Config(Map())}
			}
		}		

		def loadConfig(filename : String) = synchronized {
      val file = new File(filename)
      val absfilename = file.getCanonicalPath

			try 
			{
				//OPEN the property file
        val file_is = new FileInputStream(file)
				//make property list and load values from file
				val proplist = new Properties()
				proplist load file_is
				//Convert the keys to a set and create a Map, return the Map[String,String]
				val keys = JC.asScalaSet(proplist stringPropertyNames)
				keys foreach { key => 
					config = config ++ Map(key -> (proplist getProperty key))
				}
			} 
			catch {
				case e: Exception  => {
					if(filename != "/etc/rightfabric/config.txt") {
						//e.printStackTrace()
						Console println("Config loading ("+absfilename+") failed. Now trying to load from /etc/rightfabric")
						throw new Exception
					}
					else {
						//e.printStackTrace()
						Console println("Please provide a valid config.txt file in the start directory or specify the location file as the first parameter")
						System exit(-1)					
					}	
				}
			}
		}
		
		//Try and load a default config in the current directory
		try {loadConfig("config.txt")} catch { case e => {loadConfig("/etc/rightfabric/config.txt")}}

		def clearConfig = synchronized {config = Map("" -> ""); ()}
		
		def apply(key : String) : String = {config getOrElse(key,"")}
		def apply(key : String,default: String) : String = {config getOrElse(key,default)}
		
		
	}
}
