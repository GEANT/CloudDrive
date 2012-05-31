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
import net.vrijheid.clouddrive.exceptional._
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.httpsupport._
import java.util.{List => JList,Vector,Iterator,Date}
import net.vrijheid.clouddrive.providers._
import voldemort.versioning._
import java.io.{Serializable}

package net.vrijheid.clouddrive.sharing {

	
	class VoldemortSharing[T](implicit ctx: RootContext[T]) extends VoldemortLocker[T] with SharingLike with VMGroupsAndUsers  with Serializable {
		
		//Also get the shares client and the storage client
		val store_client = VMTalk.getStorageClient
		val share_client = VMTalk.getShareClient
		
		def setShared(path: String,sharedornot: Boolean) {
			setMetaData(path,Map("shared" -> sharedornot.toString()))
		}
		
		
		def getShared(path: String): Boolean = {
			val shared = getMetaData(path,"shared")
			shared match {
				case "true" => true
				case "false" => false
				case _ => false
			}
		}

		//Note:: these two functions deal with actual data, not links!		
		protected def setTopLevelShare(path: String,toplevel_sharedornot:Boolean) {
			
			//CODE-CC check to see if path is toplevel share folder?
			val shared_root = "/" + ctx.user + "/" + stripLeadingSlash(Config("shared_folder_root"))
			val well = ((path.startsWith (shared_root)) && (path.length == shared_root.length))
			if (well) {updateMetaData(path,Map(davEncode("toplevel_shared") -> toplevel_sharedornot.toString()))}
		}
		
		def getTopLevelShare_?(path:String):Boolean = {
			val toplevel_shared = getMetaData(path,"toplevel_shared")
			toplevel_shared match {
				case "true" => true
				case "false" => false
				case _ => false	
			}		
		}
		
		//Note:: these two functions deal with actual data, not links!
		protected def setSharedAs(source:String,folder:String) {
			updateMetaData(source,Map("sharedas" -> folder))
		}
		
		def getSharedAs(source:String):String = {
			getMetaData(source,"sharedas") match {
				case "" => pathFromFullPath(source)
				case s: String if (s != "") => s
			}
		}
		
		def isShared_?(source:String):Boolean = {
			getShared(source)
		}
		
		//Does the current user have a share underr this path name?
		def sharedNameExists(path: String): Boolean = { 
			(share_client.getValue(ctx.user)).contains(path)
		}		

		def ACLSuperSet(incoming: Map[ _ <: ACLContainer,List[String]]): Map[ _ <: ACLContainer,List[String]] = {
			//CODE-CC
			//Traverse the keys and put them in a Map(user -> permissions)
			//Users as Strings, for Groups we use the "group" list (containing the users), but exploded
			//Then, before we add we check if the user exists, if so, we merge the permissions. Otherwsie we just add.
			
			incoming.foldLeft(Map():Map[ACLContainer,List[String]]) {(finalmap,current_element) => {
				
				val container = current_element _1
				val permissions = current_element _2
				
				//local function value to test if the finalmap already containes the user
				val litmus_test = { (user: ACLUser) => finalmap.contains(user)}
				//Local function value to merge user permissions, if needed
				val merge_user_permissions: (ACLContainer,List[String]) => List[String] = {(current: ACLContainer,extra_permissions: List[String]) => {
					
					(finalmap(current) ::: extra_permissions).distinct
					
				}} 
				//local function value to add user permissions, heavily reused
				val add_user_permissions: (ACLContainer) => Map[ACLContainer,List[String]] = { a_user: ACLContainer => {
					if(litmus_test(a_user.asInstanceOf[ACLUser])) {finalmap ++ Map(a_user -> (merge_user_permissions(a_user.asInstanceOf[ACLUser],permissions)))}
					else { finalmap ++ Map(a_user -> permissions)}	
				}}

				//For every container, we add the user permissions using the above local functions
				container match {
					
					case a_user: ACLUser => {
						add_user_permissions(a_user) 
					}
					case a_group: ACLGroup => {
						a_group.group.foldLeft(Map[net.vrijheid.clouddrive.sharing.ACLContainer,List[String]]())((result,user) => {result ++ add_user_permissions(new ACLUser(user))})
					}
				}
			}}
		}

		
		def isEmptySharedFolder(path: String): Boolean  = {
			//We assume that we follow links and that theh ACL doesn't matter: the question is whether there is A user that has acces to this shared folder
			hasChildren(path)
		};
				
		def isSharedFolder(fullpath: String): Boolean = {
			stripTrailingSlash(fullpath) == "/" + ctx.user + "/" + stripLeadingSlash(Config("shared_folder_root"))
		}
		
		def isSharedFolder_?(path: String): Boolean = {
			/*share_client getValue(path) match {
				case link: VMSharedFolder => true
				case _ => false
			}*/
			inSharedFolder(followLink(path))
		}
		
		def inSharedFolder(path: String) = {
			debug("In inSharedFolder, path = " + path)
			val share_root = "/" + ctx.user + "/" + stripLeadingSlash(Config("shared_folder_root"))
			val well = ((path.startsWith (share_root)) && (path.length >= share_root.length))
			debug("path contains " + Config("shared_folder_root") + " = " + well)
			well 
		}	
		
		def sharedSource(path: String):Boolean = {
			val user = getUserNameFromPath(path)
			if (!(ctx.user == getUserNameFromPath(path)) && exists("/" + user)) { true }
			else {false}
		}

		private val update_reverse_share = {
				(current_reversed_shares:List[String],delta:String) => {
				(delta :: current_reversed_shares).distinct
			}
		}
		
		private val delete_reverse_share = {
				(current_reversed_shares:List[String],delta:String) => {
				(current_reversed_shares filterNot (List(delta).contains))
			}
		}
		
		//CODE_CC: add/remove need to check on allowedAccess as well!
		
		//add addToSharedFolder and propagate "upwards" to all of the interfaces?
		def addToSharedFolder(source: String,folder: String,newComers: Map[ _ <: ACLContainer,List[String]]) {
			//First, let's create the new ACL for this path
			val oldACL = getACL(source)
			val newACL = oldACL ++ newComers
			//Now add the shared folder
			//getSharedAs shiuld guarantee that a folder name on the toplevel stays preserved. If it's a subfolder, it gets the identical name
			addSharedFolder(source,getSharedAs(source),newACL)
		}
		
		/*The source parameter should be a full path withouth the user prefix,
		the folder is a full path that is stripped from all prepending stuff as well and can be used to share under a different name
		ACL*/
		def addSharedFolder(source: String,folder: String,ACL:Map[_ <: ACLContainer,List[String]]) {			
			
			//Use the same updater for removeSharedFolder, but then reversed. 		
			//source is the complete path, with user prefix etc.
			val source_from_user = stripTrailingSlash("/"+ctx.user + "/"+stripTrailingSlash(source))
			//Get the subtree for this folder to share all
			val shared_content = treeAsList(source_from_user).reverse
			//Mark the toplevel share (needed fof adding subtrees or changing permissions on those)
			//Also set the name under which it is shared
			setTopLevelShare(source_from_user,true)
			setSharedAs(source_from_user,folder)
			debug("Ready to add shares, entering loop")
			
			val combined_acls = ACLSuperSet(ACL)
						
			//Now, for all shared content, we process
			shared_content.foreach(
				{item} => {
				
					//We drop the length of the original path, then we have the relative path
					//We'll use this later to construct the target path 
					val relative_path = stripTrailingSlash(stripLeadingSlash(item drop (source_from_user length)))
					val now = isoDateNow()
					
					combined_acls.keys.foreach( { (key) => {
						key match {
							case a_group: ACLGroup => {
								//Double check: we only give shared to groups that exists
								if (groupExists(a_group.name)) {
									//Walk all members and add symlinks
									//First create the target path, this is the symlink directory location for the user
									a_group.group.foreach( user => {
										
										//Create target path in such a way that folder takes the place from source folder
										//This way, we have "share under name"
										debug("folder name = "+folder)
										
										//add 'folder' parameter as subcollection of shares first
										val prefix = "/"+ user +"/"+Config("shared_folder_root")+"/"
										debug("prefix = "+prefix)
																				
										//Does the root share folder name exists? If not, create it here
										if(!exists(prefix + folder)) {
											debug("Create parent folder collection "+prefix+folder)
											//Create, set ACL, update metadata to match source
											createCollection(prefix + folder)
											debug("Now setting ACL on collection" + prefix + folder)
											setACL(prefix + folder,ACL)
											debug("ACL on parent folder collection now is: " + getACL(prefix+folder))
											updateMetaData(prefix + folder,Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> now, davEncode("getlastmodified") -> now))
											updateMetaData(prefix + folder,getMetaData(item))
											debug("After updateMetaData ACL on parent folder collection now is: " + getACL(prefix+folder))
											debug("Created")
										}
										
										//No folder, add a link to a file
										debug("Now entering logic for creating a file link for user")
										val target_path = stripTrailingSlash(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path)

										debug ("relative_path = " + relative_path)
										debug("target path = "+target_path)
										//Now we can create the link. Beware to add the relative_path to the "source"

										//don't execute if target_path == prefix + folder
										if(target_path != (prefix + folder)) {
											isCollection(source_from_user + "/" + relative_path) match {
												case true => createCollection(target_path)
												case false => createLink(source_from_user + "/" + relative_path,target_path,ACL)
											}
											//Maybe we want to cache one day,  we can always comment this out to prevent an extra write to the metadata store
											setACL(target_path,ACL)	
											debug("After setACL for " + target_path + " getACL now is "+ getACL(target_path))
											updateMetaData(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path,Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> now, davEncode("getlastmodified") -> now))
											debug("After first update getACL " + target_path + " now is "+ getACL(target_path))
											updateMetaData(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path,getMetaData(item))
											debug("After second update getACL " + target_path + " now is "+ getACL(target_path))
											//Add this file to this user's shares
											//TBD (MAYBE!!!): this needs to change so we can find it quickly when a user is removed (backward link)
											//share_client applyDelta(a_user.user,target_path,update_reverse_share)							
											//share_client put(target_path,source_from_user)
										}
										
										//Old group code		
										/*debug("Now entering logic for creating link for user")
										val target_path = prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path
										debug ("relative_path = " + relative_path)
										debug("target path = "+target_path)
										//Now we can create the link. Beware to add the relative_path to the "source"
										createLink(source_from_user + "/" + relative_path,target_path,ACL)
										//Maybe we want to cache one day, so we set the ACL on the link as well
										setACL(target_path,ACL)	
										updateMetaData(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path,Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> now, davEncode("getlastmodified") -> now))
										updateMetaData(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path,getMetaData(item))*/
											

									})
								}
								
							}
					
							case a_user: ACLUser => {
								//Create target path in such a way that folder takes the place from sourcce folder
								//This way, we have "share under name"

								//We use updateMetaData in stead of set!!! Otherwise we'd overwrite....
								
								debug("folder name = "+folder)
								//add 'folder' parameter as subcollection of shares first

								val prefix = "/"+a_user.user+"/"+Config("shared_folder_root")+"/"
								debug("prefix = "+prefix)
								debug("prefix + folder = "+prefix+folder)
								debug("item = "+item)
								//Add colection if it doesn't exist, and is the toplevel share
								
								//Does the root share folder name exists? If not, create it here
								if(!exists(prefix + folder)) {
									debug("Create parent folder collection "+prefix+folder)
									//Create, set ACL, update metadata to match source
									createCollection(prefix + folder)
									debug("Now setting ACL on collection" + prefix + folder)
									setACL(prefix + folder,ACL)
									debug("ACL on parent folder collection now is: " + getACL(prefix+folder))
									updateMetaData(prefix + folder,Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> now, davEncode("getlastmodified") -> now))
									updateMetaData(prefix + folder,getMetaData(item))
									debug("After updateMetaData ACL on parent folder collection now is: " + getACL(prefix+folder))
									debug("Created")
								}
								
								
								//No folder, add a link to a file
								debug("Now entering logic for creating a file link for user")
								val target_path = stripTrailingSlash(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path)
								
								debug ("relative_path = " + relative_path)
								debug("target path = "+target_path)
								//Now we can create the link. Beware to add the relative_path to the "source"
								
								//don't execute if target_path == prefix + folder
								if(target_path != (prefix + folder)) {
									isCollection(source_from_user + "/" + relative_path) match {
										case true => createCollection(target_path)
										case false => createLink(source_from_user + "/" + relative_path,target_path,ACL)
									}
									//Maybe we want to cache one day,  we can always comment this out to prevent an extra write to the metadata store
									setACL(target_path,ACL)	
									debug("After setACL for " + target_path + " getACL now is "+ getACL(target_path))
									updateMetaData(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path,Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> now, davEncode("getlastmodified") -> now))
									debug("After first update getACL " + target_path + " now is "+ getACL(target_path))
									updateMetaData(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path,getMetaData(item))
									debug("After second update getACL " + target_path + " now is "+ getACL(target_path))
									//Add this file to this user's shares
									//TBD (MAYBE!!!): this needs to change so we can find it quickly when a user is removed (backward link)
									//share_client applyDelta(a_user.user,target_path,update_reverse_share)							
									//share_client put(target_path,source_from_user)
								}
																	
							}
						}
					}}
					)
					//Set the ACL, but now as all groups and users on the source. Handy for removal of users
					debug("now setting acl" + ACL + " on " + item)
					setACL(item,ACL)
					//Mark the source folder as shared
					setShared(item,true)
				}
			)				 
		}
		
		def removeSharedFolder(source: String,folder: String) {
			
			//"Reverse" of adding
			//CODE_CC: add ctx.user!
			//Mark the toplevel as not shared 
			//Also remove the name under which it is shared
			setTopLevelShare(source,false)
			setSharedAs(source,"")
			
			//Get the subtree for this folder to share all
			val shared_content = treeAsList(source)
			shared_content.foreach(
				{item} => {

					//We drop the length of the original path, then we have the relative path
					//We'll use this later to construct the target path to be removed 
					val relative_path = stripTrailingSlash(stripLeadingSlash(item drop (source length)))					
					val ACL = getACL(item)
					
					ACL.keys.foreach( { (key) => {
						key match {
							case a_group: ACLGroup => {
								//Double check: we only give shared to groups that exists
								if (groupExists(a_group.name)) {
									//Walk all members and add symlinks
									//First create the target path, this is the symlink directory location for the user
									a_group.group.foreach( user => {
										
										//CODE_CC Create target path in such a way that folder takes the place from sourcce folder
										//This way, we have "share under name" and we delete the correct one
										
										val target_path = "/"+user+"/"+Config("shared_folder_root")+"/"+stripTrailingSlash(stripLeadingSlash(folder)) + relative_path
										//Now we can delete the link
										delete(target_path)
										//And remove it 
										//TBD: reserach: is this redundant?
										share_client delete(target_path)
									})
								}
							}
							
							case a_user: ACLUser => {
								
								//Create target path in such a way that folder takes the place from source folder
								//This way, we have "share under name" and we delete the correct one
								val target_path = "/"+a_user.user+"/"+Config("shared_folder_root")+"/"+stripTrailingSlash(stripLeadingSlash(folder))
								//Now we can create the link
								delete(target_path)	
								//And remove it 
								//TBD: research: is this redundant?
								share_client delete(target_path)							
							}
						}
					}}
					)
					//Set the ACL to empty on the source item, this implies owner only
					setACL(item,Map())
				}
			)		
		}
		
		def changeACLOnSharedFolder(source: String,gone: List[ACLContainer]) {
			//First, let's create the new ACL for this path
			val oldACL = getACL(source)
			val newACL = oldACL.filterNot(gone.contains _)
			//Now add the shared folder
			//getSharedAs shiuld guarantee that a folder name on the toplevel stays preserved. If it's a subfolder, it gets the identical name
			//First we remove all, then we add it again under the new ACL
			//TBD: Optimize by inlining the next two calls, or trust the compiler?
			removeSharedFolder(source,getSharedAs(source))
			addSharedFolder(source,getSharedAs(source),newACL)			
		}
		
		def changeACLOnSharedFolder(source: String,gone: ACLContainer) {
			changeACLOnSharedFolder(source,List(gone))	
		}
			
		//Folder is the fully qualified path 
		def getACLOnFolder(folder: String): Map[ _ <: ACLContainer,List[String]] = {
			val path = "/" + ctx.user + "/" + Config("shared_folder_root") + "/" + stripLeadingSlash(stripTrailingSlash(folder))
			getACL(path)
		}
		
		//Just to be clear, rootpath is the path to the file
		def getACLOnFile(rootshare: String,filename: String):Map[ _ <: ACLContainer,List[String]] = {
			val path = "/" + ctx.user + "/" + Config("shared_folder_root") + "/" + stripLeadingSlash(stripTrailingSlash(rootshare)) + "/" + stripLeadingSlash(stripTrailingSlash(filename))
			getACL(path)		
		}
		
		
		def allowedAccess[T](path: String,who: String,verb: HTTPVerb[T]): Boolean = { 
			
			debug("In VoldemortSharing incoming path = "+path)
			
			//CODE_CC: make this String based.....
			val acl = getACL(path)
			var ismember = false
			var aclverb : List[String] = List()
			val owner = getUserNameFromPath(followLink(path))
			//If we own it, we're allowed everything...
			//followLink path and extract the owner, compare that -> shortcircuit if it is the owner himself
			if(owner == ctx.user) {true}
			
			else if(true) {

				//is "who" a member of the map acl
				//if so, get his acl as aclverbs
				for ((container,verbs) <- acl) {
					
					container match {
						
						case u: ACLUser => {
							if (u.user == who) {aclverb = verbs}
						}
						
						case g: ACLGroup => {
							
							//Group unfolding
							if (g.group.contains(who)) {aclverb = verbs}
							
						}
					}
				}

				aclverb.distinct
			
				//Now pattern match on the WebDAVVerb
				//Depending on the verb the user needs an ACLverb - check if he has that in ACLverbs
			
				ctx.verb match {
					case c: GET if( aclverb.contains(ACLVerb.READ)) => {true}
				
					//This is the case where we overwrite an exisiting file
					case c: PUT if ((exists(path)) && (aclverb contains ACLVerb.WRITE)) => {true}
				
					//This is the case where we overwrite an exisiting file, hence the inner match
					case c: PUT if((aclverb contains ACLVerb.WRITE ) && (aclverb contains ACLVerb.CREATERESOURCE)) => {true}
				
					case c: HEAD if (aclverb contains ACLVerb.READ) => {true}
					case c: POST if (aclverb contains ACLVerb.WRITE) => {true}
				
				
					case c: DELETE if (isCollection(path) && (aclverb contains ACLVerb.DELETECOLLECTION)) => {true}

					case c: DELETE if (isResource(path) && (aclverb contains ACLVerb.DELETERESOURCE)) => {true}
				
					//this requires READ and WRITE, hence the inner match
					case c: MOVE if ((aclverb contains ACLVerb.READ) && (aclverb contains ACLVerb.WRITE))=> {true}
				
					//this requires READ and WRITE, hence the inner match
					case c: COPY if ((aclverb contains ACLVerb.READ) && (aclverb contains ACLVerb.WRITE)) => {true}
				
					//We're always accessible for capability querying
					case c: OPTIONS => {true}
					
					case c: MKCOL if aclverb contains ACLVerb.CREATECOLLECTION => {true}
					
					case c: PROPFIND if aclverb contains ACLVerb.GETMETADATA=> {true}
					case c: PROPPATCH if aclverb contains ACLVerb.SETMETADATA => {true}
					case c: LOCK if aclverb contains ACLVerb.SETLOCK => {true}
					case c: UNLOCK if aclverb contains ACLVerb.REMOVELOCK => {true}
					//define all of the above and then some (seee ACLVerbs) based on symbol semantics for WEBSITE
					case c: WEBSITE => {
						//WATCHOUT: how do we know the action here to make decision? WEBSITE is used from the Lift side....
						//But it has an action symbol, use that
						
							if (aclverb contains ACLVerb.READ) {c.action == 'READ; true }
							else if (aclverb contains ACLVerb.WRITE) {c.action == 'WRITE; true} 
							else if (aclverb contains ACLVerb.SETMETADATA) {c.action == 'SETMETADATA; true } 
							else if (aclverb contains ACLVerb.GETMETADATA) {c.action == 'GETMETADATA; true } 
							else if (aclverb contains ACLVerb.TAKEOWNERSHIP) {c.action == 'TAKEOWNERSHIP; true } 
							else if (aclverb contains ACLVerb.MOVEOWNERSHIP) {c.action == 'MOVEOWNERSHIP; true } 
							else if (aclverb contains ACLVerb.ADDUSER) {c.action == 'ADDUSER; true } 
							else if (aclverb contains ACLVerb.REMOVEUSER)  {c.action == 'REMOVEUSER; true } 
							else if (aclverb contains ACLVerb.CREATECOLLECTION) {c.action == 'CREATECOLLECTION; true }
							else if (aclverb contains ACLVerb.CREATERESOURCE) {c.action == 'CREATERESOURCE; true }
							else if (aclverb contains ACLVerb.SETLOCK) {c.action == 'SETLOCK; true }
							else if (aclverb contains ACLVerb.REMOVELOCK) {c.action == 'REMOVELOCK; true } 
							else if (aclverb contains ACLVerb.DELETERESOURCE) {c.action == 'DELETERESOURCE ; true} 
							else if (aclverb contains ACLVerb.DELETECOLLECTION) {c.action == 'DELETECOLLECTION; true }						
							else {false}

					}
					//Silencing the compiler for wrong warnings
					case _ => {false}
				}
			}
			
			//Default to false, but this code should never be reached. OTOH, this way the ACL engine will never be breached :)
			else	
				{
					debug("ACL engine returns false, this point in the program should have never been reached")
					false
				}
		}
		
		def setACLDeep(path: String,ACL: Map[ _ <: ACLContainer,List[String]]) {
			
			treeAsList(path).foreach(node => {setACL(node,ACL)})
		}
			
	}	

	//Basically just holding some test values
 	object VMS {
		
		val testuser: String = "maarten"
		val targetuser = new ACLUser("bernard")
		val targetuser2 = new ACLUser("bernardo")
		val targetfolder: String = "supadupa"
		val aclverb = List("READ","WRITE")
		val group = new ACLGroup("testgroup",List(targetuser.user,targetuser2.user))
		val aclgroup = Map(group -> aclverb)
		val recipients = Map(targetuser -> aclverb)
	}
	
	object VMGetter {
		
		implicit val ctx = new TestContext(new TEST(Map(),'test),VMS.testuser)
		ctx.storageclient = VMTalk getStorageClient
		
		def getSharing() = {
			class VMShareHolder() extends VoldemortSharing {
				
				def doFolderShare(whichfolder: String) {
					
					debug("sharing folder with ACL"+ VMS.recipients)
					
					addSharedFolder(whichfolder,VMS.targetfolder,VMS.recipients)
				}
				
				def doFolderShareWithGroup(whichfolder: String) {
					
					debug("sharing folder with group ACL" + VMS.aclgroup)
					addSharedFolder(whichfolder,VMS.targetfolder,VMS.aclgroup)
				}
				
				def stopShare() {}
			}
			new VMShareHolder
		}
		
		def getLocker() = {
			class VMLockHolder() extends VoldemortLocker
			new VMLockHolder
		}
		
		def getMetaData() = {
			class VMMetaDataHolder() extends VoldemortMetaData
			new VMMetaDataHolder
		}
	}
	
}