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
import java.io.{Serializable}

//These are classes that cover a lot of ACLs for the storage
//In addition, the file starts with user/groups classes for ACLs as subclasses from ACLContainer
//Note that the ACLVerbs are currently not used as Voldemort seems to trip over their serialization
//We use exact String equivalents in stead for now.

package net.vrijheid.clouddrive.sharing {
	
	@SerialVersionUID(201110701L)
	abstract class ACLContainer  
	@SerialVersionUID(2011101702L)
	case class ACLUser(user: String) extends ACLContainer
	@SerialVersionUID(2011101703L)
	case class ACLGroup(name:String,group: List[String]) extends ACLContainer

    object ACLVerb {
		
		val READ = "READ"
		val WRITE = "WRITE"
		val GETMETADATA = "GETMETADATA"
		val SETMETADATA = "SETMETADATA"
		val TAKEOWNERSHIP = "TAKEOWNERSHIP"
		val MOVEOWNERSHIP = "MOVEOWNERSHIP"
		val ADDUSER = "ADDUSER"
		val REMOVEUSER = "REMOVEUSER"
		val CREATECOLLECTION = "CREATECOLLECTION"
		val CREATERESOURCE = "CREATERESOURCE"
		val SETLOCK = "SETLOCK"
		val REMOVELOCK = "REMOVELOCK"
		val DELETERESOURCE = "DELETERESOURCE"
		val DELETECOLLECTION = "DELETECOLLECTION"
		
	}

    /*@SerialVersionUID(2011092L)
	class ACLVerb(verb: String)  extends Serializable
	@SerialVersionUID(2011093L)
	class READ() extends ACLVerb("test") with Serializable 
	@SerialVersionUID(2011094L)
	class WRITE() extends ACLVerb("test") with Serializable

    @SerialVersionUID(2011097L)
	class SETMETADATA() extends ACLVerb("test") 
	@SerialVersionUID(2011097L)
	class GETMETADATA() extends ACLVerb("test") 
	@SerialVersionUID(2011098L)
	class TAKEOWNERSHIP() extends ACLVerb ("test")
	@SerialVersionUID(2011099L)
	class MOVEOWNERSHIP() extends ACLVerb ("test")
	@SerialVersionUID(20110910L)
	class ADDUSER() extends ACLVerb ("test")
	@SerialVersionUID(20110911L)
	class REMOVEUSER() extends ACLVerb ("test")



	@SerialVersionUID(20122701L)
	class CREATECOLLECTION() extends ACLVerb("test")
	@SerialVersionUID(2012271202L)
	class CREATERESOURCE() extends ACLVerb("test")
	@SerialVersionUID(2011271203L)
	class SETLOCK() extends ACLVerb ("test")
	@SerialVersionUID(2011271204L)
	class REMOVELOCK() extends ACLVerb ("test")
    @SerialVersionUID(2012271205L)
	class DELETERESOURCE() extends ACLVerb ("test")
	@SerialVersionUID(2012122706L)
	class DELETECOLLECTION() extends ACLVerb("test")
	*/

}