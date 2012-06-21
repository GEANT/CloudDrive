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
import net.vrijheid.clouddrive.pipes._
import voldemort.versioning._
import java.io.{Serializable,File,FileOutputStream,InputStream}

package net.vrijheid.clouddrive.sharing {

	
	class VoldemortSharing[T](implicit ctx: RootContext[T]) extends VoldemortLocker[T] with SharingLike with VMGroupsAndUsers  with Serializable {
		
		//Also get the shares client and the storage client
		val store_client = VMTalk.getStorageClient
		val share_client = VMTalk.getShareClient
		val groupshare_client = VMTalk.getGroups2SharesClient
		
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
			
			//Checks to see if path is toplevel share folder, then updates
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
		
		//Does the current user have a share under this path name?
		def sharedNameExists(path: String): Boolean = { 
			store_client.getValue_?(path) match {
				case s: Some[VMNode] => {
					isShared_?(path)
				}
				case None => false
			}
		}		

		def ACLSuperSet(incoming: Map[ _ <: ACLContainer,List[String]]): Map[ _ <: ACLContainer,List[String]] = {

			//Traverse the keys and put them in a Map(user -> permissions)
			//Users as Strings, for Groups we use the "group" list (containing the users), but exploded
			//Then, before we add we check if the user exists, if so, we merge the permissions. Otherwise we just add.
			
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

			inSharedFolder(followLink(path))
		}
		
		def inSharedFolder(path: String) = {
			debug("In inSharedFolder, path = " + path)
			val share_root = "/" + ctx.user + "/" + stripLeadingSlash(Config("shared_folder_root"))
			val well = ((path.startsWith (share_root)) && (path.length >= share_root.length))
			debug("path contains " + Config("shared_folder_root") + " = " + well)
			well 
		}	
		
		//This indicates that the actual resource is shared (i.e. is a link to another resource). We see this by looking at the user namespace it resides
		def sharedResource(path: String):Boolean = {
			val user = getUserNameFromPath(path)
			if (!(ctx.user == user) && exists("/" + user)) { true }
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
		
		//This will allow us to quickly map a target shared folder (e.g. one owned by the recipient side of a share) to the source.
		//This can then used when adding/removing shares
		private def getSourceFromSharedTarget(sharedfoldertarget: String): String = {
			
			//If it's a hared resource...
			sharedResource(sharedfoldertarget) match {
				
				//Get the source of the target 
				case true => {
					followLink(sharedfoldertarget)
				}
				
				//Nope, return what we got
				case false => {
					sharedfoldertarget
				}
			}
		}
				
		//Returns THE list of shared files and links. Both the source and other links.
		def getLinkedShares(inpath: String):List[String] = {
			
			//If the inpath == source, do a direct lookup on the reverse index
			//Otherwise, do a getSourceFromSharedTarget and perform the lookup on that
			val source = followLink(inpath)
			if (source == inpath) {List(inpath)}
			else {
				share_client getValue_?(inpath) match {
					case Some(parents) => parents
					case None => List()
				}
			}
		}
		
		//Returns THE list of shared files and links. Both the source and other links.
		def getLinkedGroups(inpath: String):List[String] = {
			
			//If the inpath == source, do a direct lookup on the reverse index
			//Otherwise, do a getSourceFromSharedTarget and perform the lookup on that
			val source = followLink(inpath)
			if (source == inpath) {List(inpath)}
			else {
				groupshare_client getValue_?(inpath) match {
					case Some(groups) => {
						groups 
					}
					
					case None => List()
				}
			}
		}				
		//CODE_CC: add/remove need to check on allowedAccess as well!
		
		//add addToSharedFolder and propagate "upwards" to all of the interfaces?
		def addFolderToSharedFolder(newone: String){
			
			//CODE_CC: how to deal with copying of data across back ends?
			
			//adds generic logic for adding a subfolder to an already shared parent folder, also from a "sharee" perspective and propgate the changes to other users
			
			//Logic:
			//Get the shared toplevel from the folder
			val parent_prefix = stripTrailingSlash("/"+ctx.user+getParentFolder(newone))
			val parent_source = followLink(parent_prefix)
			//Translate it to the shared "source folder". Continue only upon success
			val source = (exists(parent_source) && isSharedFolder_?(parent_source)) match {
				
				//The actual parent exists and is shared
				case true => {
					//Translate the newone to a "shared folder" and its direct parent via reversedshareindex on the parent
					val new_folder_name = fileNameFromPath(newone)
					val parent_share = parent_source + "/" + new_folder_name
					val shared_content = treeAsList(newone).reverse
					//Create the link in the "master", i.e. the orginal sharer for al content
					val source_parent_acl = getACL(parent_source)
					//Get all the linked share users for this folder, so we now how to propagate
					var other_linked_parents = getLinkedShares(parent_source)
					//Now we first copy all the content to the owner of the share, and remove it from "this" user
					//I.e. if a party invited to the srae, shares a folder it gets copied to the owner of the share and then "shared back"
					shared_content.foreach(
						(item) => {
							val relative_path = item drop(parent_prefix length)
							val new_owner = parent_source+"/"+relative_path
							
							if(new_owner != item){
								//We copy the resource data to the new owner (the "master" owner of the share). But only if it isnot the master sharer ro begin with
								copyResourceData(item,new_owner)
								//Then we delete it if necessary. Effectively we changed "mastwr owner" of the data by moving the metadata pointers
								delete(item)
								//This line protects the original source and makes sure it gets a link back per resource/collection
								other_linked_parents = newone :: other_linked_parents
							}
							
							//Get the parent's linked shares
							//Create the shared folder for the user and all other users, including reverseshareindex entries
							//We collect the users via getSourceFromSharedTarget
														
							other_linked_parents.foreach { 
								(parent) => {
								
								val new_link = parent+"/"+relative_path
								
								isCollection(new_owner) match {
									
									//It's a collection, create it
									case true => {
										createCollection(new_link)
										updateMetaData(new_link,getMetaData(new_owner))
										setACL(new_link,source_parent_acl)
										share_client applyDelta(new_owner,new_link,update_reverse_share)
									}
									
									//It's a (nested) resource, link to it
									case false => {
										createLink(new_owner,new_link,source_parent_acl)
										share_client applyDelta(new_owner,new_link,update_reverse_share)
									}
								}
							}
						}
					})									
				}
				
				case false => {
					//NO SHARE
					//Nothing or Exception, or ....
					throw new NoSuchShareException
				}
			}
		}
		
		
		def addFileToSharedFolder(newone: String){	
			
			//This uses smartCopy to deal with copying of data across back ends. See also other add* methods
			
			//adds generic logic for adding a file to an already shared parent folder, also from a "sharee" perspective and propgate the changes to other users
			
			//Logic:
			//Get the shared toplevel from the folder
			val parent_prefix = stripTrailingSlash("/"+ctx.user+getParentFolder(newone))
			val parent_source = followLink(parent_prefix)
			//Translate it to the shared "source folder". Continue only upon success
			val source = (exists(parent_source) && isSharedFolder_?(parent_source)) match {
				
				//The actual parent exists and is shared
				case true => {
					//Translate the newone to a "shared file" and its direct parent via followlink on the parent
					val new_file_name = fileNameFromPath(newone)
					val parent_share = parent_source + "/" + new_file_name
					//Create the link in the "master", i.e. the orginal sharer for al content
					val source_parent_acl = getACL(parent_source)
					//Get all the linked share users for this folder, so we now how to propagate
					var other_linked_parents = getLinkedShares(parent_source)
					val linked_groups = List()
					//Now we first copy the file to the owner of the share, and remove it from "this" user
					//I.e. if a party invited to the share, shares a file it gets copied to the owner of the share and then "shared back"
					//DELETE-IF-TRUE val relative_path = newone drop(parent_prefix length)
					//DELETE-IF-TRUE val new_owner = parent_source+"/"+relative_path
					val new_owner = parent_share
					
					if(new_owner != newone){
						//We copy the resource data to the new owner (the "master" owner of the share). But only if it is not the master sharer to begin with
						//DELETE-IF-TRUE copyResourceData(newone,new_owner)
						smartCopy(newone,new_owner)
						//Then we delete it if necessary. Effectively we changed "master owner" of the data by moving the metadata pointers
						delete(newone)
						//This line protects the original source and makes sure it gets a link back for the file (resource)
						other_linked_parents = (newone :: other_linked_parents).distinct
					}
					
					//Get the parent's linked shares 
					//Create the shared folder for the user and all other users, including reverseshareindex entries
					//We collect the users via getSourceFromSharedTarget
												
					other_linked_parents.foreach { 
						(parent) => {
						
						val new_link = parent+"/"+new_file_name
						
						isCollection(new_owner) match {
							
							//It's a collection, create it. Moer kind of resilience then correct if we end up here
							case true => {
								createCollection(new_link)
								updateMetaData(new_link,getMetaData(new_owner))
								setACL(new_link,source_parent_acl)
								//Set the reverse share
								share_client applyDelta(new_owner,new_link,update_reverse_share)
							}
							
							//It's a resource, link to it
							case false => {
								createLink(new_owner,new_link,source_parent_acl)
								share_client applyDelta(new_owner,new_link,update_reverse_share)
							}
						}
					}}
					
					linked_groups foreach {
						(group) => {
							
						}
					}									
				}
				
				case false => {
					//NO SHARE
					//Nothing or Exception, or ....
					throw new NoSuchShareException
				}
			}
			
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
					val now = idateNow()
					
					combined_acls.keys.foreach( { (key) => {
						key match {
							//Note: ACLSuperSet explosed to ACLUser -> maybe this clause is redundant.
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
											updateMetaData(prefix + folder,Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> isoDateNow(), davEncode("getlastmodified") -> now))
											updateMetaData(prefix + folder,getMetaData(item))
											debug("After updateMetaData ACL on parent folder collection now is: " + getACL(prefix+folder))
											//Add the reverse for quick lookup/delete. Map the prefix + folder (which is user specific for the receiving party) reversed to the orginal path to the data, i.e source_from_user + folder
											//TBD - change to switch the key and value here!!! 
											share_client applyDelta(source_from_user+"/"+folder,prefix + folder,update_reverse_share)
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
											updateMetaData(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path,Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> isoDateNow(), davEncode("getlastmodified") -> now))
											debug("After first update getACL " + target_path + " now is "+ getACL(target_path))
											updateMetaData(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path,getMetaData(item))
											debug("After second update getACL " + target_path + " now is "+ getACL(target_path))
											//Add this file to this user's shares
											//This changes so we can find it quickly when a user is removed (backward link)
											//Add the reverse for quick lookup/delete. Map the target_path (which is user specific for the receiving party) reversed to the orginal path to the data, i.e source_from_user + "/" + relative_path
											share_client applyDelta(source_from_user + "/" + relative_path,target_path,update_reverse_share)						
										}	
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
									updateMetaData(prefix + folder,Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> isoDateNow(), davEncode("getlastmodified") -> now))
									updateMetaData(prefix + folder,getMetaData(item))
									debug("After updateMetaData ACL on parent folder collection now is: " + getACL(prefix+folder))
									//Add the reverse for quick lookup/delete.Map the prefix + folder (which is user specific for the receiving party) reversed to the orginal path to the data, i.e prefix + folder
									share_client applyDelta(source_from_user+"/"+folder,prefix + folder,update_reverse_share)
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
									updateMetaData(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path,Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> isoDateNow(), davEncode("getlastmodified") -> now))
									debug("After first update getACL " + target_path + " now is "+ getACL(target_path))
									updateMetaData(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path,getMetaData(item))
									debug("After second update getACL " + target_path + " now is "+ getACL(target_path))
									//Add this file to this user's shares
									//This changes so we can find it quickly when a user is removed (backward link).Map the target_path (which is user specific for the receiving party) reversed to the orginal path to the data, i.e prefix + folder + relative_path
									share_client applyDelta(prefix + stripTrailingSlash(stripLeadingSlash(folder)) + "/" + relative_path,target_path,update_reverse_share)							
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
		
		
		def removeFolderFromSharedFolder(removee: String) {
			//Generic logic to remove a folder from a shared folder, also for a "sharee"

			
			//Get the parents etc so we can remove it
			val parent_prefix = stripTrailingSlash("/"+ctx.user+getParentFolder(removee))
			val parent_source = followLink(parent_prefix)
			//Translate it to the shared "source folder". Continue only upon success
			val source = (exists(parent_source) && isSharedFolder_?(parent_source)) match {
			
				//Get the shared content in the subtree
				case true => {
					val shared_content = treeAsList(removee).reverse
					//For all data, remove it...
					shared_content.foreach{(item) => {
						val sharees = getLinkedShares(item)
						//for all users that have a link
						sharees.foreach{(sharee) => {
							delete(sharee)
						}}
						//Delete the original data
						delete(item)
						//And the reverse index for the shares
						share_client delete(followLink(item))
					}}
				}
				
				case false => { throw new NoSuchShareException }
				
			}
		}
		
		def removeFileFromSharedFolder(removee: String){
			//Generic logic to remove a file from a shared folder, also for a "sharee"
			
			//Get the parents etc so we can remove it
			val parent_prefix = stripTrailingSlash("/"+ctx.user+getParentFolder(removee))
			val parent_source = followLink(parent_prefix)
			//Translate it to the shared "source folder". Continue only upon success
			val source = (exists(parent_source) && isSharedFolder_?(parent_source)) match {
			
				//Get the shared content in the subtree
				case true => {
					//For all data, remove it...
					val sharees = getLinkedShares(removee)
					//for all users that have a link
					sharees.foreach{(sharee) => {
						delete(sharee)
					}}
					//Delete the original data
					delete(removee)
					//And the reverse index for the shares
					share_client delete(followLink(removee))
				}
				
				case false => { throw new NoSuchShareException }
				
			}
			
		}
		
		
		def removeSharedFolder(source: String,folder: String) {
			
			//"Reverse" of adding
			//add ctx.user!
			val ctxuser = ctx.user
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
									//Walk all members and remove symlinks
									//First create the target path, this is the symlink directory location for the user
									a_group.group.foreach( user => {
										
										//CODE_CC Create target path in such a way that folder takes the place from source folder
										//This way, we have "share under name" and we delete the correct one
										
										val target_path = "/"+user+"/"+Config("shared_folder_root")+"/"+stripTrailingSlash(stripLeadingSlash(folder)) + relative_path
										//Now we can delete the link
										delete(target_path)
										//And remove it
										share_client applyDelta(followLink(target_path),target_path,delete_reverse_share)
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
								share_client applyDelta(followLink(target_path),target_path,delete_reverse_share)
							}
						}
					}}
					)
					//Set the ACL to empty on the source item, this implies owner only
					setACL(item,Map())
				}
			)		
		}
		
		//This copies data to a Share(d folder)
		def smartCopy(source: String,destination_share: String) {

			//First check if it is all within one user space. In which case we can copy within the same backend
			//Create an extra context and config, plus an extra storage layer based on a key (see the mkStorage signature)
			//Open both storage layers
			//Copy from source (read) to destination_share (write)
			//Close both
			
			val source_user = getUserNameFromPath(source)
			val destination_user = getUserNameFromPath(destination_share)
			
			//Make sure the current user is either the source or destination
			if(!((source_user == ctx.user)|| (destination_user == ctx.user))) {throw new NoSuchShareException }
			//Setup configs, contexts and back ends
			val source_resource = dropUserIndexFromPath(source,source_user)
			val destination_resource = dropUserIndexFromPath(destination_share,destination_user)
			val source_http_verb = new WEBSITE(Map("resource" -> source_resource),'read)
			val destination_http_verb = new WEBSITE(Map("resource" -> destination_resource),'write)
			implicit val source_context = new LiftContext(source_http_verb)
			implicit val destination_context = new LiftContext(destination_http_verb)
			source_context.userConfig = Config.userConfig(source_context)
			destination_context.userConfig = Config.userConfig(destination_context)
			source_context.storageclient = VMTalk.getStorageClient()
			destination_context.storageclient = VMTalk.getStorageClient()
			val source_storage_type = getBackend(source)
			val destination_storage_type = backendFromPath(destination_share)
			source_context.store =  Storage.mkStorage(source_storage_type,source_resource)(source_context)
			//Check: if the store types are the same, copy from the src store, get the guid and set the metadata. Most common case
			(source_storage_type == destination_storage_type) match {
				
				
				//Yep, "fast" copy
				case true => {
					
					//Copy the data using the same backend
					source_context.store open 'read
					val newguid = source_context.store copy()
					// metadata copy/... below? Including new friends get/setBackend
					createResource(destination_share)
					copyResourceData(source,destination_share)
					setBackend(destination_share,destination_storage_type)
					setOriginal(destination_share,newguid)
					
				}
				
				
				//Copy using the source backend to the destination backend
				//We use an intermediate local file so we can abstract it over all back ends using transferLocalFile
				case false => {
					destination_context.store = Storage.mkStorage(destination_storage_type,destination_resource)(destination_context)
					val src_store  = source_context.store
					val dest_store = destination_context.store
					val newguid = UUID()
					src_store.open('read)
					dest_store.open('write)
					//Download the data to a local file
					val tmpfile = File.createTempFile(newguid,".cloud")
					val outstream = new FileOutputStream(tmpfile)
					in2out(src_store.input,outstream,getPutLength(source))
					src_store close()
					outstream close()
					//Copy it to the remote backend
					dest_store.transferLocalFile(tmpfile,newguid)
					//Set the metadata etc
					copyResourceData(source,destination_share)
					setBackend(destination_share,destination_storage_type)
					setOriginal(destination_share,newguid)				}
			}
			
			//Propgate this call back in addTo... functions

		}
		
		
		//TBD: also add this for subfolders using addToSharedFolder/removeFromSharedFolder
		
		def changeACLOnSharedFolder(source: String,gone: List[ACLContainer]) {
			//First, let's create the new ACL for this path
			val oldACL = getACL(source)
			val newACL = oldACL.filterNot(gone.contains _)
			//Now add the shared folder
			//getSharedAs should guarantee that a folder name on the toplevel stays preserved. If it's a subfolder, it gets the identical name
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
			
			//This is string based via constants in the ACLVerb object - due to serialization trouble otherwise over metadata stores
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
			else {
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