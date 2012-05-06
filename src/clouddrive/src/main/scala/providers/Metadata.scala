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
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.sharing._
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.exceptional._
import net.vrijheid.clouddrive.config._
import java.io.{Serializable}

package net.vrijheid.clouddrive.providers {
	
	//
	trait MetaDataLike extends Treatise  with Serializable {
		
		def davEncode(key : String) : String 
		def isDavEncoded(nskey : String) : Boolean
		def davDecode(nskey : String) : String
		

		val tag_namespace = "xmlns:TAG=\"TAGGING:\""
		val tag_namespace_uri = "TAGGING"
		val tag_namespace_abbrev = "TAG"
		val tag_key = "alltags"
		
		def getTags(path: String): List[String] = {
			//Get the tags, split on "," and trim. Return as List[String]
			if (exists(path)) {
				((getMetaData(path,tag_key)).split(",").map (x => x.trim)).toList
			} else List()
		}
		
		def addTags(path: String,tags: List[String]) {
			//Set the new tag metadata to the concatenation of the current tag list with the new ones, unique set.
			if (exists(path)) {
				setMetaData(path,encodeNamespace(tag_namespace,tag_key),((tags ++ getTags(path)).distinct).mkString(","))
			} 
		}
		
		def setTags(path: String,tags: List[String]) {
			//Set the new tag metadata to the concatenation of the current tag list with the new ones, unique set.
			if (exists(path)) {
				setMetaData(path,encodeNamespace(tag_namespace,tag_key),tags.mkString(","))
			}
		}
		
		def addTag(path: String,tag: String) {
			//Set the new tag metadata to the concatenation of the current tag list with the new ones, unique set.
			if (exists(path)) {
				setMetaData(path,encodeNamespace(tag_namespace,tag_key),((tag :: getTags(path)).distinct).mkString(","))
			}
		}

		def removeTag(path: String,tag: String) {
			//Set the new tag metadata to the old ones with the tag removed (via filterNot)
			if (exists(path)) {
				setMetaData(path,encodeNamespace(tag_namespace,tag_key),((getTags(path).filterNot( x => x == tag))).mkString(","))
			}
		}
		
		def removeTags(path: String,tags: List[String]) {
			//Set the new tag metadata to the old ones with the tag removed (via filterNot)
			if (exists(path)) {
				setMetaData(path,encodeNamespace(tag_namespace,tag_key),((getTags(path).filterNot( x => tags.contains(x)))).mkString(","))
			}
		}
		
		def hasTags(path: String,tags: List[String]): List[String] = {
			if (exists(path)) {
				getTags(path).intersect(tags)
			} else List()
			
		}
		
		def containsTag(path: String,tag: String): Boolean = {
			if (exists(path)) {
				getTags(path).contains(tag)
			} else false
		}
		
		def containsTags(path: String,tags: List[String]): Boolean = {
			if (exists(path)) {
				getTags(path).intersect(tags) == tags
			} else false
		}
		
		def setACL(path: String,ACL: Map[ _ <: ACLContainer,List[String]])
		def getACL(path: String): Map[ _ <: ACLContainer,List[String]]
		
		def getChildren(key : String) : List[String] 
		def exists(path: String) : Boolean 

		def getMetaData(path : String): Map[String,String] 
		//Get metadata based on the abbreviation within the namespaced encoding
		//e.g. xmlns:D="DAV:" getlastmodified, becomes getlastmodified
		def getMetaData(path: String, key : String): String 
		def setMetaData(path : String,metadata : Map[String,String] ) 
		def setMetaData(path: String,key: String,value: String)
		def updateMetaData(path: String,newdata: Map[String,String])
		def updateMetaData(path: String,key: String,value: String) 		
		
		def deleteMetaData(path: String,key : String)
		def transferMetaData(source: String, destination: String) 
		def copyResourceData(source: String, destination: String)

		def getFileOwner(path: String): String
		def isFileOwner_?(path:String):Boolean 

		def hasChildren(key : String): Boolean 
		def isCollection(fullkey : String): Boolean
		def isResource(fullkey: String): Boolean = { ! isCollection(fullkey)}
		def createCollection(key : String) 
		def createResource(key : String) 

	    def getCrypto(key: String): Pair[Array[Byte],Array[Byte]] 

	    def setCrypto(key : String,crypt_key : Array[Byte], iv: Array[Byte]) 

		def getOriginal(key : String): String 
		def setOriginal(key : String,original : String) 

	    def getPutLength(key : String): Long 
	    def setPutLength(key : String,putlength : String)
	    def setPutLength(key : String,putlength : Long) 

		def delete(key : String)
		def deleteTree(key : String)
		
		//The below is an exact copy of ZooTalk (except for the implicit context added), 
		//but it uses the getChildren defined in this trait
		//It only hits resources, and not their "extras" (i.e metadata and so on)
		//This is handy for mass copy/delete/move
		private def build_list (workload: List[String], flattened: List[String]): List[String] = {
			workload match {						
				//Nothing to do, return
				case List() => {flattened}
				case _ => {
					//Get the current children (breadth first)
					val children = workload map {
						x => { 
							(getChildren(x)) map { y => {x + "/" + y}}
							}
						}
			
					//Flatten the children list, remove empty entries	
					val children_flattened = children.foldLeft (List[String]()) {
						//Apply a foldleft to create a flattened list 
						(acc : List[String],next : List[String]) =>
						{
							next match {
								//Empty entry, don't accumulate
								case List() => {acc}
								//Add the children list
								case _ => {acc ++ next}
							} 
						}
					}	
					// children_flattened is our new workload, old workload can move to flattened (breadth first)
					build_list(children_flattened,workload ++ flattened)
				}
			}
							
		}
		
		def treeAsList(path : String) = {
			
			val starters = getChildren(path)
			val treeList : List[String] = starters match {
				//Empty list, do nothing	
				case List() => { starters}
				// Execute action
				case _ => { 
					//Expand children to full path
					val expanded = starters map {x => {path + "/" + x}}
					build_list (expanded,List(path)) 
				}
			}
			(treeList ++ List(path)).distinct
		}
		
		def processTree(path : String,f : (String) => Unit) {
			(treeAsList(path)).foreach(f)
		}
	
		def nodesWithTags(path: String,tags: List[String]) = {
			(treeAsList(path)).filter( x => containsTags(x,tags))
		}
		
		def nodesWithTag(path: String,tag: String) = {
			(treeAsList(path)).filter( x => containsTag(x,tag))
		}
		
		def addTagsDeep(path: String,tags: List[String]) {
			processTree(path,{ (x) => {addTags(x,tags)}})
		}
		def addTagDeep(path: String,tag: String) {
			processTree(path,{ (x) => {addTag(x,tag)}})
		}
		
		def removeTagsDeep(path: String,tags: List[String]) {
			processTree(path,{ (x) => {removeTags(x,tags)}})
		}
		def removeTagDeep(path: String,tag: String) {
			processTree(path,{ (x) => {removeTag(x,tag)}})
		}

		def createLink(source: String,target: String,ACL:  Map[ _ <: ACLContainer,List[String]])
		def createLinkedLink(source: String,target: (String,VMLink),ACL: Map[ _ <: ACLContainer,List[String]])
		def isLink(path: String): Boolean
		def deadLink_?(path: String): Boolean = { exists(followLink(path))}
		def updateLink(link: String,newtarget: String)
		
		def isTagFolder(path: String): Boolean
		def isTagFolder_?(path: String): Boolean
		def inTagFolder(path: String): Boolean
		def followLink(path: String):String
		def createTagFolder(fullpath: String,nodes: List[String],tags: List[String],ACL: Map[ _ <: ACLContainer,List[String]])
		def createTagFolder(fullpath: String,tags: List[String],ACL: Map[ _ <: ACLContainer,List[String]])
		def getTagFolderTags(fullpath:String): List[String]
		
		def isPublicFolder(fullpath: String):Boolean
		def isCommonFolder(fullpath: String):Boolean
		
		def sync2Source(link: String,source: String)
		def sync2Link(source: String,link: String)
		def sync2Tags(user: String,path: String)
		def getTaggedLinks(user: String,path:String): List[String]
		
		def removeTagLinks(user: String,path:String) {
			 (getTaggedLinks(user,path)).foreach {(link) => delete(link)}
		}
		
		def removeFromTagSet(user: String,tagsetname: String)
		
		def forceTagFolderUpdate(user: String,foldername: String ) 
		def addFileToTagSets(user: String,path: String)
		def deleteFileFromTagSets(user: String,path: String) 

	}
		
	//
	class MetaData[T](implicit ctx: RootContext[T]) extends MetaDataLike  with Serializable  {
				
		val dav_namespace_uri = "DAV"
		val dav_namespace_abbrev = "D"
		val dav_namespace = "xmlns:D=\"DAV:\""

		@transient private val delegate = MetaDataFactory(Config("metadata_store","none"))
		
		def davEncode(key : String) : String = {
			delegate davEncode key
		}
		
		def exists(path: String): Boolean = { delegate exists path}
		
		def isDavEncoded(nskey : String) = {
			delegate isDavEncoded nskey
		}
		
		def davDecode(nskey : String) = {
			delegate davDecode nskey
		}
		
		def setACL(path: String,ACL: Map[ _ <: ACLContainer,List[String]])  {delegate.setACL(path,ACL)}
		def getACL(path: String): Map[ _ <: ACLContainer,List[String]] = {delegate.getACL(path)}
				
		def getChildren(key : String): List[String] = {
			delegate getChildren key
		}	
		
		def getMetaData(path : String): Map[String,String] = {
			delegate getMetaData path
		}
		
		//Get metadata based on the abbreviation within the namespaced encoding
		//e.g. xmlns:D="DAV:" getlastmodified, becomes getlastmodified
		def getMetaData(path: String, key : String): String = {
			delegate getMetaData(path,key)
		}
		
		def setMetaData(path : String,metadata : Map[String,String] ) {
			delegate setMetaData(path,metadata)
		}
		
		def setMetaData(path: String,key: String,value: String) {
			debug("delegate setMetaData")
			delegate setMetaData(path,key,value)
		}
		
		def deleteMetaData(path: String,key : String) {
			delegate deleteMetaData(path,key)
		}
		
		def updateMetaData(path:String,update:Map[String,String]) {
			delegate updateMetaData(path,update)
		}
		
		def updateMetaData(path: String,key: String,value: String) {
			delegate updateMetaData(path,key,value)
		}
		
		def transferMetaData(source: String, destination: String) {
			delegate transferMetaData(source,destination)
		}
		
		def copyResourceData(source: String, destination: String) {
			delegate copyResourceData(source,destination)
		}

		def hasChildren(key : String): Boolean = {
			delegate hasChildren key
		}
		
		def isCollection(fullkey : String) = {
			delegate isCollection fullkey
		}		
		
		def createCollection(key : String) {
			delegate createCollection key
		}
		
		def createResource(key : String) {
			delegate createResource key
		}	
		
	    def getCrypto(key: String): Pair[Array[Byte],Array[Byte]] = {
			delegate getCrypto key
	    }

	    def setCrypto(key : String,crypt_key : Array[Byte], iv: Array[Byte]) {
			delegate setCrypto(key,crypt_key,iv)
	    }
	
		def getFileOwner(path:String): String = {delegate getFileOwner path}
		def isFileOwner_?(path:String):Boolean = {delegate.isFileOwner_?(path)}

		def getOriginal(key : String): String = {
			delegate getOriginal key
		}		
		def setOriginal(key : String,original : String) {
			delegate setOriginal(key,original)
		}

	    def getPutLength(key : String): Long = {
      		delegate getPutLength key
	    }   
	    def setPutLength(key : String,putlength : String) {
	    	delegate setPutLength(key,putlength)
		}
	    def setPutLength(key : String,putlength : Long) {
	      delegate setPutLength(key,putlength)
	    }
    		
		def delete(key : String) {
 			delegate delete key
		}
		
		def deleteTree(key: String) { delegate deleteTree key}
		
		def createLink(source: String,target: String,ACL: Map[ _ <: ACLContainer,List[String]]) {delegate createLink(source,target,ACL)}
 		def createLinkedLink(source: String,target: (String,VMLink),ACL: Map[ _ <: ACLContainer,List[String]]) {delegate createLinkedLink(source,target,ACL)}
		def isLink(path: String) = {delegate isLink path}
		def updateLink(link: String,newtarget: String) = { delegate updateLink(link,newtarget)}
		
		def isTagFolder(path: String) = {delegate isTagFolder path}
		def isTagFolder_?(path: String) = {delegate isTagFolder_? path}
		
		def inTagFolder(path: String) = {delegate inTagFolder path}
		
		def followLink(path: String) = {delegate followLink path}
		
		def createTagFolder(fullpath: String,nodes: List[String],tags: List[String],ACL: Map[ _ <: ACLContainer,List[String]]) { delegate createTagFolder(fullpath,nodes,tags,ACL)}
		def createTagFolder(fullpath: String,tags: List[String],ACL: Map[ _ <: ACLContainer,List[String]]) { delegate createTagFolder(fullpath,tags,ACL)}
		
		def isPublicFolder(fullpath: String) = delegate isPublicFolder(fullpath)
		
		def isCommonFolder(fullpath: String) = delegate isCommonFolder(fullpath)
		
		def sync2Source(link: String,source: String) {delegate sync2Source(link,source)}
		def sync2Link(source: String,link: String) {delegate sync2Link(source,link)}
		def sync2Tags(user: String,path: String) {delegate sync2Tags(user,path)}
		def getTaggedLinks(user: String,path:String): List[String] = { delegate getTaggedLinks(user,path)}
		def forceTagFolderUpdate(user: String,foldername: String ) {delegate forceTagFolderUpdate(user,foldername)}
		def addFileToTagSets(user: String,path: String) {delegate addFileToTagSets(user,path)	}
		def deleteFileFromTagSets(user: String,path: String) {delegate deleteFileFromTagSets(user,path)}
		def removeFromTagSet(user: String,tagsetname: String) { delegate removeFromTagSet(user,tagsetname)}
		def getTagFolderTags(fullpath:String): List[String] = { delegate getTagFolderTags(fullpath)} 
		
	} 
	
	object MetaDataFactory {
		
		def get[T](what: String)(implicit ctx: RootContext[T]): MetaDataLike = {
			what match {
				//Legacy
				//case "zookeeper" => new ZooMetaData()
				case "voldemort" => new VoldemortMetaData()
				case _ => { throw new InvalidMetaDataStore}
			}
		}
		
		def apply[T](what: String)(implicit ctx: RootContext[T]): MetaDataLike = { get(what)}
		
	}
	
	
}
