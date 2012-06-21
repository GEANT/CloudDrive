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
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.providers._
import net.vrijheid.clouddrive.exceptional._
import net.vrijheid.clouddrive.httpsupport._
import java.io.{Serializable}


package net.vrijheid.clouddrive.sharing {

	//
	trait SharingLike extends LockerLike  with Serializable   {
		
		def getShared(path: String): Boolean
		def getTopLevelShare_?(path:String):Boolean 
		def getSharedAs(source:String):String
		def isShared_?(source:String):Boolean		

		def getLinkedShares(inpath: String):List[String]
		def getLinkedGroups(inpath: String):List[String]
		
		def sharedNameExists(name: String): Boolean 
		
		//Used to merge users that belong to multiple groups, so they have all their credentials
		def ACLSuperSet(incoming: Map[ _ <: ACLContainer,List[String]]):Map[ _ <: ACLContainer,List[String]]
		
		def isEmptySharedFolder(path: String): Boolean 
		
		def isSharedFolder(path: String): Boolean
		def isSharedFolder_?(path: String): Boolean
		
		def inSharedFolder(path: String): Boolean
		def sharedResource(path: String):Boolean

		def addFolderToSharedFolder(newone: String)		
		def addFileToSharedFolder(newone: String)
		def addSharedFolder(source: String,folder: String,ACL:Map[ _ <: ACLContainer,List[String]])
		
		def removeSharedFolder(source: String,folder: String)
		def removeFolderFromSharedFolder(removee: String)
		def removeFileFromSharedFolder(removee: String)

		def smartCopy(source: String,destination_share: String)
		def generateLinkForShare(sourcepath: String,targetuser: String): String  
		
		def changeACLOnSharedFolder(source: String,gone: List[ACLContainer])
		def changeACLOnSharedFolder(source: String,gone: ACLContainer)
		
		
		def getACLOnFolder(folder: String): Map[ _ <: ACLContainer,List[String]]
		def getACLOnFile(rootshare: String,filename: String):Map[ _ <: ACLContainer,List[String]]
		
		def allowedAccess[T](path: String,who: String,verb:HTTPVerb[T]): Boolean 
		
		def setACLDeep(path: String,ACL: Map[ _ <: ACLContainer,List[String]])
	}
	
	//
	class Sharing[T](implicit ctx: RootContext[T]) extends Locker[T] with SharingLike  with Serializable {
		
		@transient private val delegate: SharingLike = SharingDataFactory(Config("metadata_store","none"));
		

		def getShared(path: String): Boolean = {delegate getShared path}
		def getTopLevelShare_?(path:String):Boolean = { delegate getTopLevelShare_?(path) }
		def getSharedAs(source:String):String = {delegate getSharedAs(source)}
		def isShared_?(source:String):Boolean = {delegate isShared_? (source)}
		def sharedNameExists(name: String): Boolean = {false}
		def ACLSuperSet(incoming: Map[ _ <: ACLContainer,List[String]]):Map[ _ <: ACLContainer,List[String]] = {delegate.ACLSuperSet(incoming)}
		
		def getLinkedShares(inpath: String):List[String] = {delegate getLinkedShares(inpath)}
		def getLinkedGroups(inpath: String):List[String] = {delegate getLinkedGroups(inpath)}
		
		def isEmptySharedFolder(path: String): Boolean  = {delegate.isEmptySharedFolder(path)};
		
		def isSharedFolder(path: String): Boolean = {delegate.isSharedFolder (path)}
		def isSharedFolder_?(path: String): Boolean = {delegate.isSharedFolder_? (path)}
		
		def inSharedFolder(path: String): Boolean = delegate.inSharedFolder (path)
		def sharedResource(path: String):Boolean = { delegate sharedResource(path)}

		def addFolderToSharedFolder(newone: String) {delegate.addFolderToSharedFolder(newone)}		
		def addFileToSharedFolder(newone: String) {delegate.addFileToSharedFolder(newone)}		
		def addSharedFolder(source: String,folder: String,ACL:Map[ _ <: ACLContainer,List[String]]) {delegate.addSharedFolder(source,folder,ACL)}

		def removeSharedFolder(source: String,folder: String) {delegate.removeSharedFolder(source,folder)}
		def removeFolderFromSharedFolder(removee: String) {delegate.removeFolderFromSharedFolder(removee)}
		def removeFileFromSharedFolder(removee: String) {delegate.removeFileFromSharedFolder(removee)}		
		
		def smartCopy(source: String,destination_share: String) {delegate.smartCopy(source,destination_share)}
		def generateLinkForShare(sourcepath: String,targetuser: String): String = {delegate generateLinkForShare(sourcepath,targetuser)}
		
		def changeACLOnSharedFolder(source: String,gone: List[ACLContainer]) {delegate.changeACLOnSharedFolder(source,gone)}
		def changeACLOnSharedFolder(source: String,gone: ACLContainer) {delegate.changeACLOnSharedFolder(source,gone)}
				
		def getACLOnFolder(folder: String): Map[ _ <: ACLContainer,List[String]] = {delegate.getACLOnFolder(folder)}
		def getACLOnFile(rootshare: String,filename: String):Map[ _ <: ACLContainer,List[String]] = {delegate.getACLOnFile(rootshare,filename)}

		def allowedAccess[T](path: String,who: String,verb:HTTPVerb[T]): Boolean = {delegate.allowedAccess(path,who,verb)}
		def setACLDeep(path: String,ACL: Map[ _ <: ACLContainer,List[String]]) {delegate setACLDeep (path,ACL)}
		
	}
	
	object SharingDataFactory {
		
		def get[T](what: String)(implicit ctx: RootContext[T]): SharingLike = {
			what match {
				//Legacy
				//case "zookeeper" => new ZooLocker()
				case "voldemort" => new VoldemortSharing()
				case _ => { throw new InvalidMetaDataStore}
			}
		}
		
		def apply[T](what: String)(implicit ctx: RootContext[T]): SharingLike = { get(what)}
		
	}
	
}