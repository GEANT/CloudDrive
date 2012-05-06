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
import java.util._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.exceptional._
import java.io.{Serializable}


package net.vrijheid.clouddrive.providers {
	
	//
	trait LockerLike  extends Serializable  {
		def lock(key: String,lockdata: String): Option[String] 
		
		def unlock(key: String,lock: String)

		def getLockTime(key: String,lock: String): String

		def getLock(key : String): Option[String] 
		
		def isLocked(key : String): Boolean
		
		def lockExists(key: String,lock: String): Boolean
		
		def refreshLock(key : String,lockdata: String): String
		
		def setLockData(key : String,lock: String,data: String) 
		
		def getLockData(key : String,lock: String): String
		
		//Main interface for if header matching, for now it is uber simplifiedd. We'll add actually parsing later
		//Header, etc is in ctx
		def parseIf(key: String,token: String): Boolean 
		
		def allowed(key: String): Boolean 
	}

	class Locker[T](implicit ctx: RootContext[T]) extends MetaData[T] with LockerLike  with Serializable {
		
		//TBD: make a factory for other metadata / locker backends
		@transient private val delegate = LockerDataFactory(Config("metadata_store","none"))
		
		def lock(key: String,lockdata: String): Option[String] = {
			delegate lock(key,lockdata)
		}
		
		def unlock(key: String,lock: String) {
			delegate unlock(key,lock)
		}

		def getLockTime(key: String,lock: String) = {
			delegate getLockTime(key,lock)
		}

		def getLock(key : String): Option[String] = {
			delegate getLock(key)
		}
			
		def isLocked(key : String) = {
			delegate isLocked(key)
		}
		
		def lockExists(key: String,lock: String) = {
			delegate lockExists(key,lock)
		}
		
		def refreshLock(key : String,lockdata: String) = {
			delegate refreshLock(key,lockdata)
		}
		
		def setLockData(key : String,lock: String,data: String) {
			delegate setLockData(key,lock,data)
		}
		
		def getLockData(key : String,lock: String) = {
			delegate getLockData(key,lock)
		}
		
		//Main interface for if header matching, for now it is uber simplifiedd. We'll add actually parsing later
		//Header, etc is in ctx
		def parseIf(key: String,token: String): Boolean = {
			delegate parseIf(key,token)
		}
		
		def allowed(key: String) : Boolean = {
			delegate allowed(key)
	    }
	}
	
	object LockerDataFactory {
		
		def get[T](what: String)(implicit ctx: RootContext[T]): LockerLike = {
			what match {
				//Legacy
				//case "zookeeper" => new ZooLocker()
				case "voldemort" => new VoldemortLocker()
				case _ => { throw new InvalidMetaDataStore}
			}
		}
		
		def apply[T](what: String)(implicit ctx: RootContext[T]): LockerLike = { get(what)}
		
	}
	
}
