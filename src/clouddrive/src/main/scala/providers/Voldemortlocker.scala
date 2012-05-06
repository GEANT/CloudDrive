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
import net.vrijheid.clouddrive.control._
import java.util.{List => JList,Vector,Iterator,Date}

package net.vrijheid.clouddrive.providers {

	class VoldemortLocker[T](implicit ctx: RootContext[T]) extends VoldemortMetaData[T] with LockerLike  {
		
		
		private var cached_lock: (String,VMLock) = ("",null)
		
		//This is our function that we can pass tp VMTalk applyDelta and updates the lock part of a VMNode
		private val updateLock = {
			(node: VMNode,delta: VMLock) => {
				val newlock = delta match {
					case m: VMLock => {m}
					case null => VMLock.empty_lock
				}
				
				node match {
					case n: VMResource => {
						new VMResource(n.name,n.metadata,n.putcontentlength,n.crypto,newlock,n.original,n.versions)
					}
					case m: VMCollection => {
						new VMCollection(m.name,m.metadata,newlock,m.children)
					}	
				}
			}
		}
		
		def lock(key: String,lockdata: String): Option[String] = {
			debug("VoldemortMetaData lock:")
			val path = stripTrailingSlash(key)
			var current_node = vmstorage getValue(path)
			//This is needed for locking non existing files, e.g davfs2
			if (null == current_node) {
				debug("current_node == null")
				createResource(path)
				current_node = vmstorage getValue(path)
			}
			current_node match {
				case m: VMCollection => {debug("...collection");lockCollection(m,lockdata)}
				case n: VMResource => {debug("....resource");lockNode(n,lockdata)}
				case null => {debug("null, ...resource: returning None."); None}
			}
		}
		
		def unlock(key: String,lock: String) {
			debug("VoldemortMetaData lock:")
			val path = stripTrailingSlash(key)
			isCollection(path) match {
				case true => {debug("...collection");unlockCollection(path,lock)}
				case false => {debug("....resource");unlockResource(path,lock)}
			}
		}

		private def resetLockCache() {cached_lock = ("",VMLock.empty_lock)}
		
		private def lockTimedOut(l: VMLock): Boolean = {
			debug("VoldemortMetaData lockTimedOut")
			//First check if we should unlock the resource because of expiration (we do this inline to prevent another traversal to the vmstorage)
			val now = (new Date).getTime;
			val then = l.epoch
			((now - then ) < 600000) 
		}

		//We use this to minimize querying for locks, by caching or last lock query
		private def getRawLock(key: String): VMLock = {
			debug("Getting raw lock")
			//Clean our path
			val path = stripTrailingSlash(key)
			//Do we have the cached_lock
			((cached_lock _1) == path) match {
				//Reuse, don't travel back and forth, so don't update
				case true => {debug("Reuse cache")}
				//Nope. Get the value and update the cache (if we get a value!)
				case false => {
					debug("Update")
					//Get the node
					val vmnode = vmstorage getValue(path)
					//See if the node actually existed
					vmnode match {
						//Yes, but do we have a lock?
						case n: VMNode if (n.locks.lock == "") => {debug("reset cache, no locks"); resetLockCache}
						case m: VMNode => {cached_lock = (path,m.locks)}
						case _  => {resetLockCache}
					}
				}
			}
			cached_lock _2
		}

		def getLockTime(key: String,lock: String) = {
			debug("Getting lock time")
			val path = stripTrailingSlash(key)
			getRawLock(path) match {
				//Do we have locks?
				case m: VMNode if (m.lock == "") => {debug("no lock found, returning 0 seconds"); "Second-0"}
				//Yes
				case n: VMNode => {
					//Get the times
					val now = (new Date).getTime;
					val then = n.epoch
					((now - then ) < 600000) match {
						//If the lock is less then an hour old, return it
						case true => {
							debug("VMLock less than an hour old")
							"Second-" + (((now - then) / 1000) toString)
						}
						//Unlock, return "Second-0""
						case false => {
							debug("lock older than an hour, unlocking")
							//We update directly to prevent circular references that blow up the stack
							vmstorage applyDelta(path,null,updateLock)
							"Second-0"
						}
					}
				} 
			}
		}

		def getLock(key : String): Option[String] = {
			debug("getVMLock")
			val path = stripTrailingSlash(key)
			//Process the node
			getRawLock(key) match {
				//We have a valid node value
				//Do we have locks?
				case m: VMLock if (m.lock == "")=> {debug("No locks"); None}
				//Yes!
				case n: VMLock =>  {
					debug("locks found")
						//First check if we should unlock the resource because of expiration (we do this inline to prevent another traversal to the vmstorage)
						lockTimedOut(n) match {
						//If the lock is less then an hour old, return it
						case true => {
							debug("Returning lock")
							Some(n.lock)
						}
						//Unlock, return None
						case false => {
							debug("lock timed out, clear it")
							//We update directly to prevent circular references that blow up the stack
							vmstorage applyDelta(path,null,updateLock)
							None
						}
					}
				}
				//Key does not exist, neither does the lock
				case _ => { None }
			}
		}

		private def lockNode(node: VMNode,lockdata: String,uuid: String = (UUID())): Option[String] = {
			debug("Inside lockNode")
			//See if we have any locks
			node.locks match {
				//No locks, create one by adding a new one - simply overwriting resource.locks
				case m: VMLock if (m.lock == "") => {
					debug("no locks, adding one")
					val newlock = new VMLock((new Date).getTime,uuid,lockdata)
					vmstorage applyDelta(node.name,newlock,updateLock)
					debug("delta applied")
					Some(uuid)
				}
				//VMLock already exists, check for timeout. Otherwise return None
				case current_lock: VMLock => {
					//Check for timeout
					lockTimedOut(current_lock) match {
						//Timed out. Remove current lock and add a new one by simply overwriting resource.locks
						case true => {
							debug("lock timed out, removing")
							val newlock = new VMLock((new Date).getTime,uuid,lockdata)
							vmstorage applyDelta(node.name,newlock,updateLock)
							Some(uuid)							
						}
						case false => {
							debug("already locked")
							//As it is already locked we return None to indicate a failure to lock
							None
						}
					}
				}
			}
		} 
		
		//Utility wrapper to ease lockCollection
		private def lockNode(node: String,lockdata: String,uuid: String): Option[String] = {
			debug("In lockNode wrapper")
			val vmnode = vmstorage getValue(node)
			vmnode match {
				case n: VMNode => {lockNode(n,lockdata,uuid)}
				case _ => {None}
			}
		}
		
		
		private def lockCollection(coll: VMCollection,lockdata: String): Option[String] = {
			debug("locking collection")
			val collection = treeAsList(coll.name)
			val guid = UUID()
			collection.foreach{(item) => {lockNode(item,lockdata,guid)}}
			debug("tree processed")
			Some(guid)
		}
		
		private def unlockNode(key: String,lock: String) {
			debug ("In lockNode")
			if (lockExists(key,lock)) {
				debug("lock exists, continue")
				val oldlock = getRawLock(key)
				debug("old lock = " + oldlock)
				if (oldlock.lock == lock) {
					vmstorage applyDelta(key,null,updateLock)
					debug("locks removed")
				}
			}
		}
		
		def unlockResource(key : String, lock : String) {
			debug("in unlock, unlockResource")
			unlockNode(key,lock)
			debug("lock deleted")
		}
		
		def unlockCollection(key : String,lock: String) {
			debug("unlocking collection")
			val collection = treeAsList(key)
			collection.foreach{(item) => {unlockNode(item,lock)}}
			debug("tree processed")
		}		
		
		def isLocked(key : String): Boolean = {
			debug("Is the node locked")
			getLock(key) match {
				case None => false
				case s : Some[String] => true
			}
		}
		
		def lockExists(key: String,lock: String) = {
			debug("in lockExists")
			getLock(key) match {
				case s: Some[String] => { s.get == lock}
				case None => {false}
			}
			
		}
		
		def refreshLock(key : String,lockdata: String) = {
			debug("In refreshVMLock")
			//up the timeout (actually refresh by writing again)
			val newlock = isLocked(key) match {
				case true => {
					debug("It's locked, updateing timeout time")
					val rawlock = getRawLock(key)
					val newlock = new VMLock(new Date().getTime,rawlock.lock,rawlock.lockdata)
					vmstorage applyDelta(key,newlock,updateLock)
					Some(newlock.lock)
				}
				case false => {
					debug("not locked, create a new lock")
					lock(key,lockdata)
				}
			}
			//As we have changed it, reset the cache
			resetLockCache
			
			newlock match {
				case None => {debug("refreshed lock is None"); ""}
				case s: Some[String] => {debug("refreshed lock succesfully created"); s get}
			}
		}
		
		def setLockData(key : String,lock: String,data: String) {
			debug("In setVMLockData")
			lockExists(key,lock) match {
				case true => { 
					debug("lock exists")
					val oldlock = getRawLock(key)
					debug("old lock retrieved")
					if (oldlock.lock == lock) {
						val newlock = new VMLock(oldlock.epoch,oldlock.lock,data)
						vmstorage applyDelta(key,newlock,updateLock)
						debug("updated lockdata")
					}
				}
				case false => {debug("VMLock doesn't exist")}
			}
		}
		
		def getLockData(key : String,lock: String) = {
			debug("in getVMLockData")
			lockExists(key,lock) match {
				case true => { 
					debug("lock exists")
					val l = getRawLock(key)
					if (l.lock == lock) { debug("valid lock, returning lockdata"); l.lockdata} else {debug("locks don't match"); ""}
				}
				case false => {debug("lock doesn't exist"); ""}
			}
		}
		
		//Main interface for if header matching, for now it is uber simplifiedd. We'll add actually parsing later
		//Header, etc is in ctx
		def parseIf(key: String,token: String): Boolean = {
			debug("In parseIf")
			val ifheader = ctx.verb.header.getOrElse("If","nothing")
			debug("If header: " + ifheader)
			//Just a simple find, and anything goes... Candidate for improvement
			debug("Check if header contains token")
			ifheader contains token
		}
		
		def allowed(key: String): Boolean = {
			debug("In allowed, VoldemortMetaData")
			val token = getLock(key)
			token match {
				//No lock, so allowed
				case None => {debug("No lock on resource, allow"); true}
				//Check the lock on the If header
				case s : Some[String] => {
					debug("locked resource")
					val token_string : String = s get;
					//Our return value is the result of the If header parse and comparison with the known lock value 
					debug("See if a lock has been provided")
					parseIf(key,token_string)
				}
			}
	    }
	
		override def finalize() {
			//We need to close this one on GC.
			//WATCH
			//tagset.factory.close;
		}
	
	}
}
