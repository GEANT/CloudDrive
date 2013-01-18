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
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.sharing._
import java.util.zip._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.exceptional._

package net.vrijheid.clouddrive.providers {
	
	class VoldemortMetaData[T](implicit ctx: RootContext[T]) extends Treatise with MetaDataLike {
								
				
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
		
		private val extras = List("metadata","locks","versions","crypto","original","putlength")
		
		val vmstorage: VMClient[String,VMNode] = ctx.storageclient;
		val tagset = VMTalk getTagSetClient;
		
		def setACL(path: String,ACL: Map[ _ <: ACLContainer,List[String]]) {
			//This is the function that takes a VMNode, matches the type and merges the new metadata in
			//It is passed to applyDelta of vmstorage 
			debug("Incoming ACL in setACL = " + ACL + "for path " + path)
			val updater = {
				(oldval: VMNode,delta: Map[ _ <: ACLContainer,List[String]]) => {
					
					debug("In setACL updater delta = " + delta)
					
					oldval match {
						case n: VMResource => {
							val tmp = new VMResource(n.name,n.metadata,n.putcontentlength,n.crypto,n.locks,n.original,n.versions,delta)
							debug("new ACL set for VMResource " + tmp)
							tmp
						}
						case m: VMCollection => {
							val tmp = new VMCollection(m.name,m.metadata,m.locks,m.children,delta)
							debug("new ACL set for VMCollection " + tmp)
							tmp
						}
						
						case p: VMLink => {
							val tmp = new VMLink(p.destination,delta,p.name,p.metadata)
							debug("new ACL set for VMLink " + tmp)
							tmp
						}
					}
				}
			}
			vmstorage applyDelta(path,ACL,updater)			
		}
		
		def getACL(path: String): Map[_ <: ACLContainer,List[String]] = {
			//vmstorage.getValue_?(followLink(path)) match {
			vmstorage.getValue_?(path) match {
				case None => Map()
				case Some(acl: VMNode) => {
					acl.ACL
				}
			}
		}
		
		def getChildren(key : String) : List[String] = {
			debug("In getChildren (VoldemortMetaData), key = " + key)
			debug ("Checking vmstorage " + (vmstorage == null))
			val ubernode = vmstorage getValue(stripTrailingSlash(key))
			debug("Retrieved the 'ubernode' = " + ubernode)
			ubernode match {
				
				case node: VMCollection => {
					node.children.map {
						s => {
							stripTrailingSlash(s.substring(findIndexOfReverse(s,"/"),s.length))
						}
					}
				}
				
				case _ => { List()}
				
			}
		}	

		def exists(path: String): Boolean = { 
			debug ("In VoldemortMetaData exists")
			!( null == (vmstorage getValue(stripTrailingSlash(path))))
		}
		
		def getMetaData(path : String): Map[String,String] = {
			
			debug("In getMetaData (Map) for VoldemortMetaData, path = " + path)
			vmstorage.getValue(stripTrailingSlash(path)) match {

				case node: VMNode => {node.metadata}
				case null => {
					debug("no metadata")
					Map()
				}
			}
		}
		
		
		//Get metadata based on the abbreviation within the namespaced encoding
		//e.g. xmlns:D="DAV:" getlastmodified, becomes getlastmodified
		def getMetaData(path: String, key : String): String = {
			debug("In getMetaData (key) for VoldemortMetaData, key = " + key)
			//Now we need to find the key that maps, first get all metadata
			val metadata = getMetaData(path)
			//Filter all the children to get the one we need, then extract it (it is a List with lenght 1)
			val matched_child = ((metadata.keys.filter { (x) => { (decodeNamespace(x) _2) == key}}) toList)
			debug(matched_child)
			//Check to make sure it is not empty
			if (!(matched_child isEmpty)){
				debug("matched_child exists")
				debug(metadata(matched_child(0)))
				metadata(matched_child(0))
			} else {
				debug("matched_child does not exist")
				""
			}
		}
		
		def updateMetaData(path: String,newdata: Map[String,String]) {
			
			//This is the function that takes a VMNode, matches the type and merges the new metadata in
			//It is passed to applyDelta of vmstorage 
			val updater = {
				(oldval: VMNode,delta: Map[String,String]) => {
					oldval match {
						case n: VMResource => {
							val newdata = oldval.metadata ++ delta
							new VMResource(n.name,newdata,n.putcontentlength,n.crypto,n.locks,n.original,n.versions,n.ACL)
						}
						case m: VMCollection => {
							debug("In updateMetaData, new metadata = "+delta)
							val newdata = oldval.metadata ++ delta
							new VMCollection(m.name,newdata,m.locks,m.children,m.ACL)
						}
						
						case p: VMLink => {
							val newdata = oldval.metadata ++ delta
							new VMLink(p.destination,p.ACL,p.name,newdata)
						}
					}
				}
			}
			vmstorage applyDelta(path,newdata,updater)
		}
		
		def updateMetaData(path: String,key: String,value: String) {
			
			//This is the function that takes a VMNode, matches the type and merges the new metadata in
			//It is passed to applyDelta of vmstorage 
			val updater = {
				(oldval: VMNode,delta: (String,String)) => {
					//Note that the order is important on the right hand - the last Map in a ++ overwrites the key if necessary
					val newdata = oldval.metadata ++ Map ((delta _1) -> (delta _2))
					oldval match {
						case n: VMResource => {
							new VMResource(n.name,newdata,n.putcontentlength,n.crypto,n.locks,n.original,n.versions,n.ACL)
						}
						case m: VMCollection => {
							new VMCollection(m.name,newdata,m.locks,m.children,m.ACL)
						}
						
						case p: VMLink => {
							new VMLink(p.destination,p.ACL,p.name,newdata)
						}
					}
				}
			}
			vmstorage applyDelta(path,((key,value)),updater)
		}
		
		def setMetaData(path : String,metadata : Map[String,String] ) {
			debug("In setMetaData (Map)" + metadata)
			//Note that this fails if the resource does not exist
			updateMetaData(stripTrailingSlash(path),metadata)
		}
		
		def setMetaData(path: String,key: String,value: String) {
			debug ("In setMetaData (key/value)")
			updateMetaData(stripTrailingSlash(path),key,value)
		}
		
		def deleteMetaData(path: String,key : String) {
			val fullpath = stripTrailingSlash(path)
			debug("In deleteMetaData (key)")
			//This is the function that takes a VMNode, matches the type and merges the new metadata in
			//It is passed to applyDelta of vmstorage 
			val updater = {
				(oldval: VMNode,delta: String) => {
					val newdata = oldval.metadata - delta
					oldval match {
						case n: VMResource => {
							new VMResource(n.name,newdata,n.putcontentlength,n.crypto,n.locks,n.original,n.versions,n.ACL)
						}
						case m: VMCollection => {
							new VMCollection(m.name,newdata,m.locks,m.children,m.ACL)
						}
					}
				}
			}
			vmstorage applyDelta(path,key,updater)			
		}
		
		def transferMetaData(source: String, destination: String) {
			setMetaData(destination,(getMetaData(source)))
		}
		
		def copyResourceData(source: String, destination: String) {
			debug ("in copyResourceData, VoldemortMetaData")
			
			val source_path = stripTrailingSlash(source)
			val destination_path = stripTrailingSlash(destination)
			//Get the source value, and create a new one based on that
			val dest_value : VMNode = vmstorage getValue source_path match {
				
				case m: VMCollection => {
					debug("in copyResourceData, new VMCollection")
					new VMCollection(destination_path,m.metadata,m.locks,List(),m.ACL)
				}
				
				case n: VMResource => {
					debug("in copyResourceData, new VMResource")
					new VMResource(destination_path,n.metadata,n.putcontentlength,n.crypto,n.locks,n.original,n.versions,n.ACL)
				}
			}
			debug("dest_value created")
			//Bluntly put this value, as we copy (thus we overwrite)
			vmstorage put(destination_path,dest_value)			
		}

		def hasChildren(key : String): Boolean = {
			//Speaks for itself
			((getChildren(key) length) == 0)
		}
		
		def isCollection(fullkey : String) = {
			debug("isCollection path = " + fullkey)
			vmstorage getValue fullkey match {
				case m: VMCollection => {true}
				case _ => {false}
			}
		}	
		
		//Get the current file owner
		def getFileOwner(path: String): String = {
			getUserNameFromPath(followLink(path))
		}
			
		//Is the current user the owner
		def isFileOwner_?(path:String):Boolean = {
			getFileOwner(path) == ctx.user
		}		
		private def addToCollection(newkey: String) {
			debug("Adding to VoldemortMetaData collection")
			val localkey = stripTrailingSlash(newkey)
			debug("localkey = " + localkey)
			val parent = stripTrailingSlash(localkey.substring(0,findIndexOfReverse(localkey,"/")))
			debug("parent =" + parent)
			//Our updater function
			val updater = {
				(coll: VMNode,delta: String) => {
					coll match {
						case col: VMTagFolder => { 
							coll.asInstanceOf[VMTagFolder]
							new VMTagFolder(col.tags,col.ACL,col.name,(delta :: col.children).distinct,col.metadata)
						}
						case col: VMCollection => {
							coll.asInstanceOf[VMCollection]
							new VMCollection(col.name,col.metadata,col.locks,(delta :: col.children).distinct)
						}
					}
				}
			}
			
			vmstorage applyDelta(parent,localkey,updater)
		}
		
		private def removeFromCollection(oldkey: String) {
			debug("Removing from VoldemortMetaData collection")
			val localkey = stripTrailingSlash(oldkey)
			val parent = stripTrailingSlash(localkey.substring(0,findIndexOfReverse(localkey,"/")))
			debug("localkey, parent = " + localkey + " , " + parent)
			//Our updater function
			val updater = {
				(coll: VMNode,delta: String) => {
					coll match {
						case col: VMTagFolder => { 
							coll.asInstanceOf[VMTagFolder]
							new VMTagFolder(col.tags,col.ACL,col.name,(col.children.filterNot(_ == delta)).distinct,col.metadata)
						}
						case col: VMCollection => {
							coll.asInstanceOf[VMCollection]
							new VMCollection(col.name,col.metadata,col.locks,(col.children.filterNot(_ == delta)).distinct)
						}
					}
				}
			}
			

			
			vmstorage applyDelta(parent,localkey,updater)		
		}
		
		def createCollection(key : String) {
			
			//Add it to the parent. If that fails, we're outta here
			if(exists(pathFromFullPath(key))) {
				addToCollection(key)
				//Initialize the new key
				vmstorage init(key,new VMCollection(key,Map(),VMLock.empty_lock,List()))
			}
			else throw new NoParentCollectionException
		}
		
		def createResource(key: String) {
			//Add it to the parent. If that fails, we're outta here
			addToCollection(key)
			vmstorage init(key,new VMResource(key,Map(),"0",(Array(),Array()),VMLock.empty_lock,"",List()))
		}	
		
	    def getCrypto(key: String): Pair[Array[Byte],Array[Byte]] = {
	      debug("In getCrypto")
	      vmstorage getValue(stripTrailingSlash(key)) match {
		  		case m: VMResource => {m.crypto}
	        	case _ => { Pair(Array[Byte](),Array[Byte]())}
			}
	    }

	    def setCrypto(key : String,crypt_key : Array[Byte], iv: Array[Byte]) {
			
			val local_key = stripTrailingSlash(key)
			
			val updater = {
				(oldval: VMNode,delta: (Array[Byte],Array[Byte])) => {
					oldval match {
						case n: VMResource => {
							new VMResource(n.name,n.metadata,n.putcontentlength,delta,n.locks,n.original,n.versions)
						}
						case m: VMCollection => {
						 	oldval
						}
					}
				}
			}
			
			vmstorage applyDelta(local_key,(crypt_key,iv),updater)
	    }

		def getOriginal(key : String): String = {
	      vmstorage getValue(stripTrailingSlash(key)) match {
		  		case m: VMResource => {m.original}
	        	case _ => { ""}
			}						
		}	
		def setOriginal(key : String,original : String) {
			
			val local_key = stripTrailingSlash(key)
			
			val updater = {
				(oldval: VMNode,delta: String) => {
					oldval match {
						case n: VMResource => {
							new VMResource(n.name,n.metadata,n.putcontentlength,n.crypto,n.locks,delta,n.versions)
						}
						case m: VMCollection => {
						 	oldval
						}
					}
				}
			}
			
			vmstorage applyDelta(local_key,original,updater)	
		}

	    def getPutLength(key : String): Long = {
      
			vmstorage getValue(stripTrailingSlash(key)) match {
		  		case m: VMResource => {m.putcontentlength.toLong;}
	        	case _ => { 0.asInstanceOf[Long]}
			}
	    }   
	    
		def setPutLength(key : String,putcontentlength : String) {
			
			val local_key = stripTrailingSlash(key)
			
			val updater = {
				(oldval: VMNode,delta: String) => {
					oldval match {
						case n: VMResource => {
							new VMResource(n.name,n.metadata,delta,n.crypto,n.locks,n.original,n.versions)
						}
						case m: VMCollection => {
						 	oldval
						}
					}
				}
			}
			
			vmstorage applyDelta(local_key,putcontentlength,updater)
 	    }
	
	    def setPutLength(key : String,putlength : Long) {
	      setPutLength(key,(putlength toString))
	    }
    		
		def delete(key : String) {
			debug("deleting for " + key)
			//Remove any links to this key from tag folders
			if(!(isLink(key))) {
				debug("try and find connected links")
				getTaggedLinks(ctx.user,key).foreach{
					(link) => {
						debug("Deleting link: "+link)
						removeFromCollection(link)
					}
				}
			}
			//Remove it from parent
			removeFromCollection(key)
			debug(key + "removed from parent node")
			vmstorage delete(key)
			debug("done, " + key + " deleted.")
		}
		
		def deleteTree(key: String) {
			processTree(key,delete _)
		}
		
		def createLink(source: String,target: String,ACL: Map[ _ <: ACLContainer,List[String]] = Map()) {
			debug("In createLink")
			val metadata = getMetaData(source)
			val name = target.slice(findIndexOfReverse(stripTrailingSlash(target),"/"),target.length)
			val link = new VMLink((source,null),ACL,name,metadata)
			debug("adding "+target+" to collection")
			addToCollection(target)
			vmstorage put(target,link)
		}
		
		def createLinkedLink(source: String,target: (String,VMLink),ACL: Map[ _ <: ACLContainer,List[String]]) {
			debug("In createLinkedLink")
			val metadata = getMetaData(source)
			val name = (target _1).slice(findIndexOfReverse(stripTrailingSlash(target _1),"/"),(target _1).length)
			//for deep links
			val link = new VMLink(target,ACL,name,metadata)
			debug("adding "+(target _1)+" to collection")
			addToCollection(target _1)
			vmstorage put(target _1,link)
		}
				
		def isLink(path: String) = {
			vmstorage getValue(path) match {
				case link: VMLink => true
				case _ => false
			}
		}
		
		def updateLink(linkpath: String,newtarget: String) = { 
			vmstorage getValue(linkpath) match {
				case link: VMLink => {
					val newlink =  new VMLink((newtarget,null),link.ACL,link.name,link.metadata)
					vmstorage put(linkpath,newlink)
				}
				case _ => false
			}			
		}
		
		
		/* (Correct) behaviour: if it's a link, return the resolved link
		   Otherwise: return the original path as it isn't a link
		*/
		def followLink(path: String): String = {
			vmstorage getValue(path) match {
				//Note: we translate this to the VMkey, the destination is relative
				case link: VMLink => {
					link.destination _2 match {
						case l: VMLink => {followLink(l.destination _1)}
						case _ => {link.destination _1}
					}
				}
				//No link, just return the original path
				case _ => path
			}			
		}
		
		def isTagFolder_?(path: String) = {
			vmstorage getValue(path) match {
				case link: VMTagFolder => true
				case _ => false
			}
		}
		
		def inTagFolder(path: String) = {
			debug("In inTagFolder, path = " + path)
			val tag_root = "/" + ctx.user + "/" + stripLeadingSlash(Config("tag_folder_root"))
			val well = ((path.startsWith (tag_root)) && (path.length >= tag_root.length))
			debug("path contains " + Config("tag_folder_root") + " = " + well)
			well 
		}
			
		
		def userless_source(user: String,source: String) = {source.slice(1 + user.length,source.length)}
		
		//Notethat fullpath is absolute, i.e. includes "user" namespace
		def createTagFolder(fullpath: String,nodes: List[String],tags: List[String],ACL: Map[ _ <: ACLContainer,List[String]] = Map()) {
			
			debug("In createTagFolder")
			
			//A list with names of fully qualified "sources" and "targets"
			val links: List[(String,String)] = (
				for (source <- nodes) yield {
					val userless = userless_source(ctx.user,source)
					val name = source.slice(findIndexOfReverse(stripTrailingSlash(source),"/"),source.length)
					//val name = stripLeadingSlash(source)
					val target = deDupeName(ctx.user,stripTrailingSlash(fullpath) + "/" + name,userless)
					//val target = stripTrailingSlash(fullpath) + "/" + name
					((source,target))					
				}
			)
			debug("VMLinks created" + links)
			
			val foldername = fullpath.slice(
				findIndexOfReverse(stripTrailingSlash(fullpath),"/"),
				fullpath.length)
			debug("Display name = "+foldername)
		
			val tagfolder = new VMTagFolder(tags,ACL,foldername,List(),Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") ->isoDateNow(), davEncode("getlastmodified") -> idateNow()))
			debug("Tag folder object created: "+tagfolder)
			
			//First put the tagfolder in place and set its metadata
			vmstorage put(fullpath,tagfolder)
			debug("Stored tag folder object under key "+fullpath)
					
			addToCollection(fullpath)
			
			debug("Type is now "+vmstorage.getValue(fullpath).getClass)
			
			//First create all the links
			links.foreach((k_v) => { 
				debug("looping over source/target pairs")
				debug("Key = " + (k_v _1))
				createLink(k_v _1,k_v _2)
			})			
			debug("Stored all VMLinks")			
			
			//Check for existance of tagsets
			tagSets_?(ctx.user) match {
				//Yes, update
				case true => {
					addToTagSet(ctx.user,((foldername,tags)))
				}
				
				//No, so let's create one
				case false => {
					createTagSet(ctx.user,Map(foldername -> tags))
				}
			}

		}	
	
		def createTagFolder(fullpath: String,tags: List[String],ACL: Map[ _ <: ACLContainer,List[String]]) {
			
			debug("In createTagFolder")
			val foldername = fullpath.slice(
				findIndexOfReverse(stripTrailingSlash(fullpath),"/"),
				fullpath.length)
			debug("Display name = "+foldername)
		
			val tagfolder = new VMTagFolder(tags,ACL,foldername,List(),Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> isoDateNow(), davEncode("getlastmodified") -> idateNow()))
			debug("Tag folder object created: "+tagfolder)
			
			//First put the tagfolder in place and set its metadata
			vmstorage put(fullpath,tagfolder)
			debug("Stored tag folder object")
					
			addToCollection(fullpath)
			debug("Forcing an update on the folder")
			forceTagFolderUpdate(ctx.user,fullpath)
			debug("Done")
						
		}
		
		def isPublicFolder(fullpath: String):Boolean = {stripTrailingSlash(fullpath) == "/" + ctx.user + "/" + stripLeadingSlash(Config("public_folder_root"))}
		
		def isCommonFolder(fullpath: String):Boolean = { stripTrailingSlash(fullpath) == "/" + ctx.user + "/" + stripLeadingSlash(Config("common_folder_root"))}
		
		def isTagFolder(fullpath: String):Boolean = { stripTrailingSlash(fullpath) == "/" + ctx.user + "/" + stripLeadingSlash(Config("tag_folder_root"))}
		
		/* Doe this user have a tagset defined at all */
		private def tagSets_?(user: String): Boolean = {
			tagset getValue_? user match {
				case Some(set) => true
				case None => false
			}
		}
		
		/* create a tag set for this user */
		private def createTagSet(user: String,newset: Map[String,List[String]]) {
			tagset init(user,newset)
		}
		
		//Use this with VMTalk.applyDelta to update a complete tagset
		private val updateTagSetMinus = {
			(oldset: Map[String,List[String]],setname:String) => {
				oldset - setname;
			}
		}
		
		//Use this with VMTalk.applyDelta to update a complete tagset
		private val updateTagsInTagSetMinus = {
			(oldset: Map[String,List[String]],delta: (String,String)) => {
				val setname = delta _1
				val tag = delta _2
				val current = oldset.getOrElse(setname,List())
				oldset + ((setname,current filterNot(_ == tag)))
			}
		}
		
		
		//Use this with VMTalk.applyDelta to update a complete tagset
		private val updateTagSetPlus = {
			(oldset: Map[String,List[String]],newelement: (String,List[String])) => {
				oldset + newelement;
			}
		}
		
		//Use this with VMTalk.applyDelta to update a complete tagset
		private val updateTagsInTagSetPlus = {
			(oldset: Map[String,List[String]],delta: (String,String)) => {
				val setname = delta _1
				val tag = delta _2
				val current = oldset.getOrElse(setname,List())
				oldset + ((setname,(tag :: current).distinct))
			}
		}
				
		/*This will return the user's tagset, a set of all saved tag searches
		as map foldername -> tags
		*/
		private def getTagSet(user: String): Map[String,List[String]] = {
			tagset getValue_?(user) match {
				case Some(set) => set
				case None => Map()
			}
		}
		
		/* Add a new folder to the tagset */
		private def addToTagSet(user: String,newelement: (String,List[String])) {
			tagset applyDelta(user,newelement,updateTagSetPlus)
		}
		
		/* Ad a specific tag to the set of tags for a specific folder */
		private def addTagToTagSet(user: String,setname: String,tag: String) {
			tagset applyDelta(user,((setname,tag)),updateTagsInTagSetPlus)
			forceTagFolderUpdate(user,setname)
		}
		
		/* Remove a folder from the tag set */
		def removeFromTagSet(user: String,tagsetname: String) {
			tagset applyDelta(user,tagsetname,updateTagSetMinus)
		}

		/* Remove a specific tag from a specific folder */
		private def removeTagfromTagSet(user: String,setname: String,tag: String) {
			tagset applyDelta(user,((setname,tag)),updateTagsInTagSetMinus)
			forceTagFolderUpdate(user,setname)
		}
				
		/* Bluntly set a tagset for a user */
		private def setTagSet(user: String,set: Map[String,List[String]]) {
			tagset put(user,set)
		}
		
		//Return the tagsets (folder names) for a given user/tags combo
		private def tagSetFor(user:String,tags: List[String]) = {
			debug("In tagSetFor user = "+user+" tags = "+tags)
			val tagset = getTagSet(user)
			debug("tagset = "+tagset)
			(tagset.filter{
				(keyvalue) => { 
					debug("tags in keyvalue filter = "+(keyvalue _2))
					(keyvalue _2 ).intersect(tags) == (keyvalue _2)
				}
			}).keys.toList
		}
		
		private def deDupeName(user: String,link_path:String,source_path:String): String = {
			link_path + " points to source file "+source_path.replaceAll("/","---")+""
		}
		
		/* For adding a new file to current tagsets based on its search results */
		def addFileToTagSets(user: String,path: String) {		
			
			debug("In addFileToTagSets")
			val filename = fileNameFromPath(path)
			val userless = userless_source(user,path)
			
			//val filename = stripLeadingSlash(path.slice(1 + user.length,path.length))
			debug("Filename = "+filename)
			getTagSet(user) match {
				
				case tagset: Map[String,List[String]] if (!tagset.isEmpty) => {
					val current_tags = getTags(path)
					debug("current_tags = "+current_tags)
					//create intersection for every tagset, compute tagfolders
					//dedupeName and add...
					var taglinks: List[String] = List()
					tagset.foreach{
							(key_value) => {
							//See if we have the same tags
							if ((key_value _2).intersect(current_tags) == (key_value _2)) {
								taglinks = deDupeName(user,"/" + user + "/" + stripLeadingSlash(Config("tag_folder_root")) + "/" + (key_value _1) + "/" + filename,userless) :: taglinks
							}
						}
					}
					debug("All taglinks to be created: "+taglinks)
					taglinks.foreach(link => createLink(path,link))
				}
				//No tagset, so nothing to add...
				case _ => {debug("No tagesets found")}
			}
		
		}
		
		def deleteFileFromTagSets(user: String,path: String) {
			debug("In deleteFileFromTagSets, deleting links for path = "+path)
			getTaggedLinks(user,path).foreach{
				(link) => {
					debug("in deleteFileFromTagSets foreach delete loop, link = "+link)
					try {delete(link)} catch {case _ => {}}
				}
			}
		}
		/* Sync the tags FROM link TO source */
		def sync2Source(link: String,source: String) {
			debug("In sync2Source")
			if(exists(link) && exists(source)) {
				//Propagate tags from link to source
				setTags(source,getTags(link))
			}				

		}
		/* Sync the metadata and tags FROM source  TO Link */
		def sync2Link(source: String,link: String) {
			debug("In sync2Link")
			if(exists(link) && exists(source)) {
				//Propagate tags from link to source
				setMetaData(link,getMetaData(source))
			}			
		}
		/* When tags on a file are updated
		call this to make sure it ends up in the right tag sets */
		def sync2Tags(user: String,path: String) {
			debug("In sync2Tags, source path =  "+path)
			//TBD optimize using removeFromTagSet, addTagToTagSet, getTagSet and set computations.
			deleteFileFromTagSets(user,path)
			debug("deleted "+path+"from tagsets")
			addFileToTagSets(user,path)
			debug("added "+path+" to tagsets" )
		}		
			
		
		/* Get a List of all the paths that link to the supllied paths from Tag folders */
		def getTaggedLinks(user: String,path:String): List[String] = {
			debug("In getTaggedLinks")
			//Get the basic variables
			val source_tags = getTags(path)
			debug("source_tags = "+source_tags)
			val filename = fileNameFromPath(path)
			debug("filename = "+filename)
			//Compute all paths by filtering the tags in every tagset. They have to satisfy the condition that the set intersection with the user's tagset == user's tagset.
			//Then we fetch the keys of the filtered map and transform them to paths in the tagroot.
			val usertagsets = tagSetFor(user,source_tags)
			debug("tagSetFor user = "+usertagsets)
			val tagfolders = (usertagsets).map{
				(x) => 
				{
					"/" + user + "/" + stripLeadingSlash(Config("tag_folder_root")) + "/" + x
				}
			}
			debug("All found tag folders are: "+ tagfolders)
			var resultset: List[String] = List()
			tagfolders.foreach(tagfolder => {
				debug("Adding tagfolders, full links in foreach tagfolders loop")
				//Try and find it directly
				val fulltagpath = deDupeName(user,tagfolder +"/" + filename,userless_source(user,path))
				//Some debug staements before we add it to the resultset
				debug("fulltagpath ="+fulltagpath)
				debug("followedLink for "+fulltagpath+" is "+followLink(fulltagpath))
				debug("adding directly")
				resultset = fulltagpath :: resultset 
				debug("resultset now is "+resultset)
			})
			debug("All resulting links are: "+ resultset)
			resultset
		}
		
		
		/* Force a search update on a specific tag folder (expensive) */
		def forceTagFolderUpdate(user: String,fullpath: String ) {
			
			debug("In forceTagFolderUpdate")
			
			if(isTagFolder_?(fullpath)) {
				debug("We have a tag folder")
				val current_folder = vmstorage.getValue(fullpath).asInstanceOf[VMTagFolder]
				val tags = current_folder.tags
				val ACL = current_folder.ACL
				//Get all nodes that have "our" tags
				val nodes = treeAsList("/" + user).filterNot{ (x) => {
							(isCollection(x) || isLink(x))
						}
					}.filter{ (x) =>
						{containsTags(x,tags)}
					}
				
				debug("Nodes are: "+nodes)
				
				//Create a list of source,target link parameters
				val links: List[(String,String)] = (
					for (source <- nodes) yield {
						val userless = userless_source(user,source)
						val name = source.slice(findIndexOfReverse(stripTrailingSlash(source),"/"),source.length)
						val target = deDupeName(ctx.user,stripTrailingSlash(fullpath) + "/" + name,userless)
						((source,target))					
					}
				)
				val foldername = fullpath.slice(
					findIndexOfReverse(stripTrailingSlash(fullpath),"/"),
					fullpath.length)
				debug("Display name = "+foldername)

				val tagfolder = new VMTagFolder(tags,ACL,foldername,List(),Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> isoDateNow(), davEncode("getlastmodified") -> idateNow()))
				vmstorage put(fullpath,tagfolder)
				links.foreach((k_v) => { 
					debug("looping over source/target pairs")
					debug("Key = " + (k_v _1))
					createLink(k_v _1,k_v _2)
				})	
				
				//Check for existance of tagsets
				tagSets_?(ctx.user) match {
					//Yes, update
					case true => {
						addToTagSet(ctx.user,((foldername,tags)))
					}

					//No, so let's create one
					case false => {
						createTagSet(ctx.user,Map(foldername -> tags))
					}
				}			
			}
		}
	
		def getTagFolderTags(fullpath:String): List[String] = {
			vmstorage getValue(fullpath) match {
				case a: VMTagFolder => {
					a.tags
				}
				case _ => List()
			}
		}
		
		override def finalize() {
			//We need to close this one on GC.
			//WATCH
			//tagset.factory.close;
		}
	} 

	object VoldemortMetaData
	{
		
	}


}
