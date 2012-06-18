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
import scala.collection.{JavaConversions => JC}
import java.util.{List => JList,Vector,Iterator,Date}
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.sharing._
import voldemort.client._
import voldemort.store.{InsufficientOperationalNodesException}
import voldemort.versioning._
import net.vrijheid.clouddrive.exceptional._
import scala.collection.JavaConversions._
import java.io.{Serializable}

package net.vrijheid.clouddrive.utils { 
	
	//
	trait StoreDelta[K,V] extends StoreClient[K,V]   {
		
		def applyDelta[D](key: K,delta: D, newValue: (V,D) => V)
		def init(key: K,value: V) : Versioned[V]
	}
	

	class VMClient[K,V](delegate: StoreClient[K,V]) extends Treatise with StoreDelta[K,V] with Serializable   {
		
		//var factory : StoreClientFactory = _
		
		private val maxtries = (Config("max_tries","1")) toInt;

		//FIX_CC Add method for dispatching failures after max_tries to a log/recovery system by
		//send newValue  and the key there. This can only be done when we're near the end, as it will
		//involve e.g. function serialization (maybe even closures). Hard and subtle stuff with #cases depending
		//On the rest of the code.		
		override def applyDelta[D](key: K,delta: D,newValue: (V,D) => V) {
			debug("applying delta")
			//nifty code here for trying to apply a delta maxtries times
			var tried = 0
			var updated = false
			var next_update: Versioned[V] = get_?(key) match {
				case Some(v) => v
				case None => {throw new VoldemortWrapperException}
			}
			//Guard against null values from Voldemort
			if (! (null == next_update)) {
				next_update setObject newValue(next_update getValue,delta);
				debug("update delta(versioned): " + next_update.toString)
			
				//We are goint to try maxtries until updated
				while (!((tried > maxtries) || (updated))) {
					tried += 1
					try {
						//This wil throw an exception if our data is stale
						put(key,next_update)
						updated = true
						debug("delta applied")
					}
					catch {
						//Stale data, let's try and reconcile
						case o : ObsoleteVersionException => {
							debug("ObsoleteVersionException, retry")
							get_?(key) match {
								case Some(v) => {
									v setObject(newValue(v getValue,delta))
									next_update = v
								}
								case None => {throw new VoldemortWrapperException}
							}
						}
						
						//This is needed for a bug in Voldemort 0.81, see
						//http://groups.google.com/group/project-voldemort/browse_thread/thread/6073a4e362720a42
						//TBD: We should be able to remove this once we upgrade (actually: we MUST remove it, probably)
						//NOTE: we have upgraded, but leave it here for backwrds compatibility
						case p: InsufficientOperationalNodesException => {
							get_?(key) match {
								case Some(v) => {
									v setObject(newValue(v getValue,delta))
									next_update = v
								}
								case None => {throw new VoldemortWrapperException}
							}							
						}
					}
				}
			}
			//This will also be thrown if the key didn't exist 
			if (! updated) {throw new UpdateFailedException}
			debug("applied delta to Voldemort store")
		}
		
		override def applyUpdate(action: UpdateAction[K,V]) = { delegate applyUpdate(action) }
	 	override def applyUpdate(action: UpdateAction[K,V],maxTries: Int) = { delegate applyUpdate(action,maxTries) }
	 	override def delete(key: K) = {delegate delete key}
	 	override def delete(key: K,version: Version) = {delegate delete(key,version)}
	 	
		def get_?(key: K): Option[Versioned[V]] = {
			val res = delegate get key
			res match {
				case v: Versioned[V] => Some(v)
				case _ => None
			}
		}
	 	override def get(key: K): Versioned[V] = {delegate get(key)}
		override def get(key: K,defaultValue: Versioned[V]) = {delegate get(key,defaultValue)}
		override def get(key: K, transforms: java.lang.Object ) = {delegate get(key,transforms)}
	 	override def getAll(keys: java.lang.Iterable[K]) = {delegate getAll keys}
	 	override def getAll(keys: java.lang.Iterable[K],transforms: java.util.Map[K,java.lang.Object]) = {delegate getAll(keys,transforms)}
	 	override def getResponsibleNodes(key: K)  = {delegate getResponsibleNodes key}
	    def getValue_?(key: K): Option[V] = {
			val res = delegate getValue key
			res match {
				case v: V => Some(v)
				case _ => None
			}
		}
	 	override def getValue(key: K): V = {delegate getValue (key)}
	 	override def getValue(key: K,defaultValue: V): V = {delegate getValue (key,defaultValue)}
	
		override def init(key: K,value: V): Versioned[V] = {
			var versioned = new Versioned(value)

			get_?(key) match {
				//Already exists, so init is redundant
				case Some(v) => {
					debug("key exists, fetched. Done.")
					//But we can always try and update. Effectiveky-re-init
					//val updater = (value: V,delta: V) => {
					//	delta
					//} 
					//applyDelta(key,value,updater)
					//get(key)
				}
				//Good, we prevent empty keys, initialize
				case _ => {
					debug("key does not exists, creating with default value")
					try {put(key,versioned)} 
					catch { 
						case e => {
							versioned = get_?(key) match {
								case Some(v) => v
								case None => {throw new VoldemortWrapperException}
							}
						}
					}
				}
			}

			debug("In VMClient.init: " + versioned toString)
			versioned
		}
		
	 	override def put(key: K,value: V) = {delegate put(key,value)}
	 	override def put(key: K,versioned: Versioned[V]) = {delegate put(key,versioned)}
	 	override def putIfNotObsolete(key: K,versioned: Versioned[V]) = {delegate putIfNotObsolete(key,versioned)}
		override def put(key: K,  value: V, transforms: java.lang.Object ) = {delegate put(key,value,transforms)}
				
	}
	

	//
	@SerialVersionUID(2011031L)
	class GBMonth(var total_size: Long,var usage: Long)  extends Serializable 
	
	//
    @SerialVersionUID(2011032L)  
	trait Quotum  extends Serializable {
		
		var storage_quotum: Long
		var gbmonth_quotum: Long
		var network_in_quotum: Long
		var network_out_quotum: Long
		var request_count_quotum: Long
		
		def quotumReached(used: GBMonth): Boolean
		def quotumPercentage(used: GBMonth): Int
		def quotumReached(used: GBMonth,in: Long,out: Long,rc: Long): Boolean
		def quotumPercentage(used: GBMonth,in: Long,out:Long,rc: Long): Int
		
	}
	
	//
	@SerialVersionUID(2011033L)
	class VMLock(val epoch: Long,val lock: String,val lockdata: String)  extends Serializable 
	object VMLock {
		val empty_lock = new VMLock(0,"","")
	}
 	
	//
    @SerialVersionUID(2011034L)
	class ReverseLock(val user : String)  extends Serializable 
		
		
	//	
    @SerialVersionUID(2011035L)
	class StorageQuotum extends Quotum with Serializable   {
		var storage_quotum: Long = 0
		var gbmonth_quotum: Long  = 0
		var network_in_quotum: Long  = 0
		var network_out_quotum: Long  = 0
		var request_count_quotum: Long  = 0
		
		def quotumReached(used: GBMonth) = {quotumReached(used,0,0,0)}
		def quotumReached(used: GBMonth,in: Long,out: Long,rc: Long) = {
			used.total_size > storage_quotum
		}
		
		def quotumPercentage(used: GBMonth) = {quotumPercentage(used,0,0,0)}
		def quotumPercentage(used: GBMonth,in: Long,out: Long,rc: Long) = {
			(1 + (100 * used.total_size / storage_quotum)) toInt;
		}
	}

	//Interface for Storage Linked list structure
	//
    @SerialVersionUID(2011036L) 
	abstract class VMNode(val name: String,val metadata: Map[String,String],val locks: VMLock,val ACL: Map[ _ <: ACLContainer,List[String]])  extends Serializable 
	
	//
	@SerialVersionUID(2011037L) 
	class VMCollection(override val name: String,override val metadata: Map[String,String],override val locks: VMLock,val children: List[String],override val ACL: Map[ _ <: ACLContainer,List[String]] = Map()) extends VMNode(name,metadata,locks,ACL) with Serializable  ;
	
	//
	@SerialVersionUID(2011038L) 
	class VMResource(override val name: String,override val metadata: Map[String,String],val putcontentlength: String,val crypto: (Array[Byte],Array[Byte]),override val locks: VMLock,val original: String,val versions: List[String],override val ACL: Map[ _ <: ACLContainer,List[String]] = Map()) extends VMNode(name,metadata,locks,ACL) with Serializable  ;
	//Note that destination is without the user path, i.e. like the verb resource.
	//Or in other words: the filesystem path, not the Voldemort key (which is "/" + user + destination)
    //
	@SerialVersionUID(2011039L) 
	class VMLink(val destination: (String,VMLink),override val ACL: Map[ _ <: ACLContainer,List[String]],override val name: String,override val metadata: Map[String,String]) extends VMNode(name,metadata,VMLock.empty_lock,ACL) with Serializable ;
	
	//
    @SerialVersionUID(20110310L) 
	class VMTagFolder(val tags: List[String],override val ACL: Map[ _ <: ACLContainer,List[String]],override val name: String,override val children: List[String],override val metadata: Map[String,String]) extends VMCollection(name,metadata,VMLock.empty_lock,children,ACL) with Serializable ;
	
	//Seems unnecessary, we use VMLinks exclusively...
	//@SerialVersionUID(20110912L) 
	//class VMSharedFolder(override val ACL: Map[ACLContainer,List[String]],override val name: String,override val children: List[String],override val metadata: Map[String,String]) extends VMCollection(name,metadata,VMLock.empty_lock,children,ACL) with Serializable ;
		
	object VMTalk extends Treatise {
		
		var storageClient: VMClient[String,VMNode] = null
		var shareClient: VMClient[String,List[String]] = null
		var authNclient: VMClient[String,Map[String,String]] = null
		var incomingDataVMClient: VMClient[String,Long] = null
		var outgoingDataVMClient: VMClient[String,Long] = null
		var requestCounterClient: VMClient[String,Long] = null
		var GBMonthVMClient: VMClient[String,GBMonth] = null
		var QuotaVMClient: VMClient[String,Quotum] = null
		var ReverseLockClient: VMClient[String,ReverseLock] = null
		var tagSetClient: VMClient[String,Map[String,List[String]]] = null
		var groupClient: VMClient[String,List[String]] = null
		var userToGroupsClient:VMClient[String,List[String]] = null
		var guestclient: VMClient[String,Map[String,String]] = null		 
		
		val bootstrapUrls = Config("voldemort","tcp://localhost:6666")
		//TBD tweak ClientConfig, how many threads/client
		val bootlist = bootstrapUrls.split(",").map(_.trim).toList;
		debug("bootlist = "+bootlist)
		//TBD: Maybe make this configurable via config
		val clientconfig = new ClientConfig
		clientconfig.setBootstrapUrls(bootlist)
		clientconfig.setMaxBootstrapRetries(5)
		//clientconfig.setMaxThreads(512)
		clientconfig.setMaxQueuedRequests(5000)
		//clientconfig.setMaxTotalConnections(25000)
		//clientconfig.setMaxConnectionsPerNode(9000)
		//clientconfig.setSelectors(512)
		//clientconfig.setEnablePipelineRoutedStore(true)
		//clientconfig.setEnableJmx(false)
		//val factory = new CachingStoreClientFactory(new SocketStoreClientFactory(clientconfig));		
		val factory = new SocketStoreClientFactory(clientconfig);		
		
        def getStorageClient() = {
                 if (storageClient == null) {
                         debug("Getting VM storage client")
                         //val factory = new SocketStoreClientFactory(clientconfig);             
                         val tmp: StoreClient[String,VMNode] = factory getStoreClient("storage")
                         storageClient = new VMClient[String,VMNode](tmp)
                 }
                 debug("storageClient - null?" + (storageClient == null))
                 storageClient 
                 
         }

        def getShareClient() = {
                 if (shareClient == null) {
                         debug("Getting VM share client")
                         //val factory = new SocketStoreClientFactory(clientconfig);             
                         val tmp: StoreClient[String,List[String]] = factory getStoreClient("reverseshareindex")
                         shareClient = new VMClient[String,List[String]](tmp)
                 }
                 debug("shareClient - null?" + (shareClient == null))
                 shareClient 

         }

		def getAuthnClient() = {
			debug("Getting VM authn client")
			//val factory = new SocketStoreClientFactory(clientconfig);		
			if (authNclient == null) {
			
				val tmp: StoreClient[String,Map[String,String]] = factory getStoreClient("authn")
				authNclient = new VMClient[String,Map[String,String]](tmp)
				//client.factory = factory
			}			
			authNclient
		} 
				
		def getIncomingMeterer() = {
			if (incomingDataVMClient == null) {
				debug("Getting VM incoming client")
			 	//val factory = new SocketStoreClientFactory(clientconfig);		
				val tmp: StoreClient[String,Long] = factory getStoreClient("bandwidth-in")
				incomingDataVMClient = new VMClient[String,Long](tmp)
				//incomingDataVMClient.factory = factory
				incomingDataVMClient
			}
			debug("incomingDataVMClient - null?" + (incomingDataVMClient == null))
			
			incomingDataVMClient
		} 
		
		def getOutgoingMeterer() = {
			if (outgoingDataVMClient == null) {
				debug("Getting VM outgoing client")
				//val factory = new SocketStoreClientFactory(clientconfig);		
				val tmp: StoreClient[String,Long] = factory getStoreClient("bandwidth-out")
				outgoingDataVMClient = new VMClient[String,Long](tmp)
				//outgoingDataVMClient.factory = factory
			}
			debug("outgoingDataVMClient - null?" + (outgoingDataVMClient == null))
			
			outgoingDataVMClient
		} 	
			
		def getRequestMeterer() = {
			if (requestCounterClient == null) {
				debug("Getting VM request client")
				//val factory = new SocketStoreClientFactory(clientconfig);		
				val tmp: StoreClient[String,Long]= factory getStoreClient("requests")
				requestCounterClient = new VMClient[String,Long](tmp)
				//requestCounterClient.factory = factory
			}
			debug("requestCounterClient - null?" + (requestCounterClient == null))
			
			requestCounterClient
		} 
		
		def getGBMonthMeterer() = {
			if (GBMonthVMClient == null) {
				debug("Getting VM request client")
				//val factory = new SocketStoreClientFactory(clientconfig);		
				val tmp: StoreClient[String,GBMonth] = factory getStoreClient("storage")
				GBMonthVMClient = new VMClient[String,GBMonth](tmp)
				//GBMonthVMClient.factory = factory
			}
			debug("GBMonthVMClient - null?" + (GBMonthVMClient == null))
			
			GBMonthVMClient
		}

        def getQuotaMeterer() = {
                 if (QuotaVMClient == null) {
                         debug("Getting VM quota client")
                         //val factory = new SocketStoreClientFactory(clientconfig);             
                         val tmp: StoreClient[String,Quotum] = factory getStoreClient("quotas")
                         QuotaVMClient = new VMClient[String,Quotum](tmp)
                         //QuotaVMClient.factory = factory
                 }
                 debug("QuotaVMClient - null? " + (QuotaVMClient == null))
                 
                 QuotaVMClient
         }

        def getReverseLocker() = {
                if (ReverseLockClient == null) {
                        debug("Getting VM request client")
                        //val factory = new SocketStoreClientFactory(clientconfig);             
                        val tmp: StoreClient[String,ReverseLock] = factory getStoreClient("reverse-lock")
						debug("tmp reverse lock client from factory= "+tmp)
                        ReverseLockClient = new VMClient[String,ReverseLock](tmp)
						debug("Now converted to ReverseLockClient")
                        //ReverseLockClient.factory = factory
                }
                debug("ReverseLockClient - null? " + (ReverseLockClient == null))
                
                ReverseLockClient
                
        }

        def getTagSetClient() = {
                if (tagSetClient == null) {
                        debug("Getting VM request client")
                        //val factory = new SocketStoreClientFactory(clientconfig);             
                        val tmp: StoreClient[String,Map[String,List[String]]] = factory getStoreClient("tagset")
                        tagSetClient = new VMClient[String,Map[String,List[String]]](tmp)
                        //tagSetClient.factory = factory
                }
                debug("tagSetClient - null?" + (tagSetClient == null))
                
                tagSetClient
        }

		def getGroupClient() = {
			if (groupClient == null) {
				debug("Getting groupClient")
				//val factory = new SocketStoreClientFactory(clientconfig);		
				val tmp: StoreClient[String,List[String]] = factory getStoreClient("groups")
				groupClient = new VMClient[String,List[String]](tmp)
				//tagSetClient.factory = factory
			}
			debug("groupClient - null?" + (groupClient == null))
			
			groupClient
		}
		
		
		def getUserToGroupClient() = {
			if (userToGroupsClient == null) {
				debug("Getting groupClient")
				//val factory = new SocketStoreClientFactory(clientconfig);		
				val tmp: StoreClient[String,List[String]] = factory getStoreClient("user2groups")
				userToGroupsClient = new VMClient[String,List[String]](tmp)
				//tagSetClient.factory = factory
			}
			debug("userToGroupsClient - null?" + (userToGroupsClient == null))
			
			userToGroupsClient
		}	
			
		def getGuestClient() = {
			debug("Getting VM guest client")
			//val factory = new SocketStoreClientFactory(clientconfig);		
			if (guestclient == null) {
			
				val tmp: StoreClient[String,Map[String,String]] = factory getStoreClient("guests")
				guestclient = new VMClient[String,Map[String,String]](tmp)
				//client.factory = factory
			}			
			guestclient
		}
	}
	
}
