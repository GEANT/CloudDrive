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
import providers.MetaData
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.control._
import voldemort.versioning._

//Use providers.MetaData here??

package net.vrijheid.clouddrive.setup {


	
	object VMSetup extends Treatise {	
	 	
		//Slight duplication wrt MetaData, but saves mixing it in
		val dav_namespace_uri = "DAV"
		val dav_namespace_abbrev = "D"
		val dav_namespace = "xmlns:D=\"DAV:\""		
		
		def davEncode(key : String) : String = {
			encodeNamespace(dav_namespace,key)
		}
		
		def isDavEncoded(nskey : String) = {
			(extractNamespaceURI(nskey) == (extractNamespaceURI(dav_namespace)))
		}
		
		def davDecode(nskey : String) = {
			val (ns,key) = decodeNamespace(nskey)
			if (isDavEncoded(ns)) { extractNamespace(ns) + ":" + key}
			else {throw new Exception("Not a DAV namespace")}
			
		}			
		
		
		def setStorageQuota(user: String,quotum: Int) {
			val k : Long = 1024
			val gb: Long = (k * k * k) 
			val s_quotum = new StorageQuotum()
			s_quotum.storage_quotum = (quotum * gb)
			val key = userYear(user)
			val qvm = VMTalk getQuotaMeterer()
			qvm put(key,s_quotum)
			//qvm.factory.close
		}
		
		private def initGBMonth(user: String)(implicit gbm: VMClient[String,GBMonth]): Versioned[GBMonth] = {
			
			debug("In initGBMonth")
			//The idea is that we get the previous month, read it and set the current months totals that way
			//if there is no previous month, then this month is the first, and we initialize it with zeros
			val current_month = userMonthYear(user)
			val previous_month = previousUserMonthYear(user)
			val previous_gb_month = gbm get_? previous_month match {
				case Some(v) => v
				case None => {
					//First month
					debug("new month, first time")
					gbm init(current_month,new GBMonth(0,0))					
				}
			}

			val gbmonth = previous_gb_month getValue;
			debug("updating usage for new month")
			gbmonth.usage = (gbmonth.total_size * hoursOfThisMonth)
			debug("... and init on the new month with the updated values")
			gbm init(current_month,gbmonth)

		}
		
		
		def initMeteringInfra(who: String) {
			val moi_now = userMonthYear(who)
			val moi_year = userYear(who)
			//Get client for storing in voldemort
			val incoming_meterer = VMTalk getIncomingMeterer;
			//Initialize or current value
			incoming_meterer init(moi_now,0L)
			incoming_meterer init(moi_year,0L)
			
			val outgoing_meterer = VMTalk getOutgoingMeterer;
			//Initialize or current value
			outgoing_meterer init(moi_now,0L)
			outgoing_meterer init(moi_year,0L)
			//And add to the incoming meterer
			
			//Now, we're going to up the rquest counter
			val request_meterer = VMTalk getRequestMeterer;
			//Initialize or current value
			//val request_count : Int = 1 + (request_meterer getvalue moi)
			//And add to the incoming meterer
			request_meterer init(moi_now,0L)
			request_meterer init(moi_year,0L)
			
			implicit val gbmonth_meterer = VMTalk getGBMonthMeterer;
			initGBMonth(who)
			//incoming_meterer.factory.close;
			//outgoing_meterer.factory.close;
			//request_meterer.factory.close;
			//gbmonth_meterer.factory.close;
		}

		def activateDavDrive(user: String,password: String,config: Map[String,String]) {
			val vmclient = VMTalk.getAuthnClient;
			
			val realm = Config("realm","beneathclouds.com")
			val ha1 = (md5(user + ":" + realm + ":" + password)) toLowerCase
			val data: Map[String,String] = config + (("ha1",ha1))
			val current_data  = vmclient get_?(user) match {
				case Some(v) => v.getValue
				case None => Map[String,String]()
			}			
			vmclient.put(user,(current_data ++ data))
			//vmclient.factory.close;
		}
		
		def createRootFolder(folder: String,user: String) {
			
			debug("In createRootFolder")
			
			val fs_root = "/" + user
			//Ensure we start with  a leading slash
			val newfolder = folder startsWith ("/") match {
				case true => folder
				case false => "/" + folder
			}
			
			val fullpath = fs_root + newfolder
			//Create the WEBSITE with files path
			val verb = new WEBSITE(Map("resource" -> newfolder),'prop )
			implicit val ctx = new LiftContext(verb)
			ctx.user = user
			ctx.userConfig = Config.userConfig(ctx)
			ctx.storageclient = VMTalk.getStorageClient;
			//Set the metadata
			val metadata = new MetaData()
			metadata.createCollection(fullpath)
			debug("collection " + fullpath + " added.")
			
			val now = idateNow()
			metadata.setMetaData(fullpath,Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> now, davEncode("getlastmodified") -> now))
			debug("....and metadata set.")			
			
		}

		def addUser(user: String,config: Map[String,String],guest: Boolean = false) {
			
			val vmclient = VMTalk.getAuthnClient;
			val data = guest match {
				case false => Map("role" -> "user") ++ config 
				case true => Map("role" -> "guest") ++ config
			}
			vmclient.put(user,(data))	
			
			//Set it to 10GB default
			setStorageQuota(user,10)
			//Create a root file system
			val storage_client = VMTalk.getStorageClient;
			val fs_root = "/" + user
			val root_metadata = Map(
			   davEncode("creationdate") -> idateNow(),
			   davEncode("displayname") -> "",
			   davEncode("getlastmodified") -> idateNow(),
			   davEncode("resourcetype") -> "<D:collection/>"
			)
			storage_client put(fs_root,new VMCollection(fs_root,root_metadata,VMLock.empty_lock,List()))

			//Create some root folders
			List(Config("public_folder_root"),Config("tag_folder_root"),Config("common_folder_root"),Config("shared_folder_root")).foreach( x => createRootFolder(x,user))
			//Init the metering infra
			initMeteringInfra(user)
			//vmclient.factory.close;
			//storage_client.factory.close;
		}
		
		def deleteUser(user: String) {

			val verb = new WEBSITE(Map("resource" -> "/"),'prop )
			implicit val ctx = new LiftContext(verb)
			ctx.user = user
			ctx.userConfig = Config.userConfig(ctx)
			ctx.storageclient = VMTalk.getStorageClient;
			val metadata = new MetaData()
			try {metadata.deleteTree("/" + user)} catch {case _ =>{}}
			
			val vmclient = VMTalk.getAuthnClient;
			vmclient delete user;
			//vmclient.factory.close;
			//TBD: delete metering info
			//val storage_client = VMTalk.getStorageClient;
			//val fs_root = "/" + user
			//storage_client delete(fs_root)
		}

		
	}
}