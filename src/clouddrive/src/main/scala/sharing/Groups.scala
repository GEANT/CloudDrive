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
import net.vrijheid.clouddrive.sharing._
import net.vrijheid.clouddrive.providers._
import net.vrijheid.clouddrive.exceptional._


package net.vrijheid.clouddrive.sharing {
	
	//Group names are global, add ctx.user in front?

	trait GroupsAndUsers extends Treatise {
		
		
		def getUsers(group: String):List[String] 
		
		def getGroups(user: String): List[String] 
		
		//Does the group exist?
		def groupExists(group: String):Boolean 
		
		def addGroup(group: String,users: List[String] = List()) 
		
		def isGroupEmpty(group: String):Boolean 
		
		def deleteEmptyGroup(group: String) 
		
		def deleteGroup (group: String) 

		def addGuestUser(user: String,config: Map[String,String]) 
		
		def deleteGuestUser(user: String) 
		
		def addUsertoGroup(group: String,user: String) 
				
		def deleteUserFromGroup(group: String,user: String) 
		
		def groupsForUser(user: String): List[String] 
		
		def deleteUserFromAllGroups(user: String) 
		
		def userInGroup(group:String,user:String): Boolean 
		
		def userExists(user: String):Boolean 
		
		def addUser(user: String)
	}

	
	trait VMGroupsAndUsers extends GroupsAndUsers {
		
		//Get our group client, and the reverse mapping
		val u2gclient = VMTalk.getUserToGroupClient
		val g2uclient = VMTalk.getGroupClient
		val guest_client = VMTalk.getGuestClient
		val groups_share_client = VMTalk.getGroups2SharesClient
		val me_share_client = VMTalk.getShareClient
		
		var sharer: SharingLike = _

		private val update_group_share = {
				(group_shares:List[String],delta:String) => {
				(delta :: group_shares).distinct
			}
		}
		
		private val delete_group_share = {
				(group_shares:List[String],delta:String) => {
				(group_shares filterNot (List(delta).contains))
			}
		}
		
		def getUsers(group: String):List[String] = {
			if(groupExists(group)) {g2uclient.getValue(group)}
			else List()
		}
		
		def getGroups(user: String): List[String] = {
			if(userExists(user)) {u2gclient getValue user}
			else List()
		}
		
		//Does the group exist?
		def groupExists(group: String):Boolean = {
		
			g2uclient.getValue_?(group) match {
				//We use Option and have a value: So, yes....
				case s: Some[List[String]] => true			
				//or no.....
				case None => false
			}
		}
		
		def addGroup(group: String,users: List[String] = List()) {
			
			groupExists(group) match {
				
				//We're trying to re-add a group.....
				case true => {throw new GroupExistsException}
				//Add a group using init (double protection that way)
				
				case false => {
					g2uclient init(group,users)
					//Make sure the users are added and add this group
					users.foreach({
						user => {
							addUser(user)
							addUsertoGroup(group,user)
						}
					})
				}
			}
		}
		
		def isGroupEmpty(group: String):Boolean = {
			
			//Check to see if the group has members
			g2uclient.getValue_?(group) match {
				
				//We still have members, so not empty
				case s: Some[List[String]] => false
				//...we don't. We're empty.
				case None => true
			}
		}
		
		def deleteEmptyGroup(group: String) {
			
			//Is the group empty
			isGroupEmpty(group) match {
				//Yes, delete it
				case true => {g2uclient delete group}
				//Nope, runtime exception
				case false => {throw new GroupNotEmptyException}
			}
		}
		
		def deleteGroup (group: String) {
			
			//FIrst who, is still a member
			val members = g2uclient getValue group;
			//Remove this group for these users from their groups list
			//This also removes the groups from the users list via deleteUserFromGroup
			members.foreach(member => deleteUserFromGroup(group,member))
			//Now, delete the group (effectively empty)
			g2uclient delete group;	
			//1 Get the shares for this group's users
			//2 Per share: see if a user has access (loop over users)
			//YES: delete share/permissions
			//NO: do nothing		
		}

		def addGuestUser(user: String,config: Map[String,String]) {
			
			//Try and the user safely
			guest_client.init(user,config)
		}
		
		def deleteGuestUser(user: String) {
			val groups = groupsForUser(user)
			groups.foreach(group => deleteUserFromGroup(group,user))
			guest_client delete user
		}
		
		def addUsertoGroup(group: String,user: String) {
			
			//This is the function that takes a VMNode, matches the type and merges the new metadata in
			//It is passed to applyDelta of vmstorage 
			val updater = {
				(oldval: List[String],delta: String) => {
					(delta :: oldval).distinct
				}
			}
			
			if(groupExists(group)) {
				//make sure the user exists in the group system
				addUser(user)
				//Add the user to the group
				g2uclient.applyDelta(group,user,updater)
				//add user to reverse map via u2gclient
				u2gclient.applyDelta(user,group,updater)
				//CODE_CC add shares for group to user space!!!!
				//1 Get the shares for this group
				groups_share_client getValue_?(group) match {
					
					case Some(shares) => {
						//2 Per share: see if the user is already receiving the share as user
						shares.foreach{
							(share) => {
								//Determine the ACL (note that it is the same, as the group already exists in the ACL by definition, i.e. we add a user to a group and look at the EXISTING shares of the group)
								val ACL = sharer.asInstanceOf[MetaDataLike].getACL(share)
								var newACL: Map[_ <:ACLContainer,List[String]] = Map()
								var userAlreadyHasShare = false
								ACL.foreach{
									(pair) => {
										val aclcontainer = pair _1
										val aclacl = pair _2;
										if ((aclcontainer.isInstanceOf[ACLUser]) && (aclcontainer.asInstanceOf[ACLUser].user == user)){
											userAlreadyHasShare = true
										}
										else {newACL = newACL ++ Map(aclcontainer -> aclacl)}
									}
								}
								
								if (userAlreadyHasShare) {
									//handle deduplication, no need to add the new link as the user already has it
									//val newACL = ACL - acluser.asInstanceOf[ACLContainer]
									//Set it on the master share
									sharer.asInstanceOf[MetaDataLike].setACL(share,newACL)
									//Update the ACL on all the referenced sharees
									//Get all the links, update their ACLs
									sharer.getLinkedShares(share).foreach({
										(shard) => {sharer.asInstanceOf[MetaDataLike].setACL(shard,newACL)}
									})
								} else {
									//We only need to create a new link and add it to the reversed share index
									//Create a new link for the new user, with the new ACL
									val target = sharer.generateLinkForShare(share,user)
									sharer.asInstanceOf[MetaDataLike].createLink(share,target,ACL)
								}
							}
						}					
					}
					
					case None => {
							//NOOP
						}
				}
			}			
		}
				
	 	def deleteUserFromGroup(group: String,user: String) {
			
			val updater = {
				(oldval: List[String],delta: String) => {
					val (left,right) = oldval.span(_ != user)
					left ::: right.drop(1)
				}
			}

			val updater2 = {
				(oldval: List[String],delta: String) => {
					val (left,right) = oldval.span(_ != group)
					left ::: right.drop(1)
				}
			}
			
			//1 Get the shares for this group's users
			//2 Per share: see if a user has access (loop over users)
			//YES: delete share/permissions
			//NO: do nothing
			
			/** In detail
			Get the shares for this group from groups_share_client
			For each share: 
			- get the reverse share from me_share_client
			- fetch the link for this user
			- remove the link in this users's shared... space
			- remove the link from the reverse index in me_share_client
			
			*/
			
			if(userInGroup(group,user)) {

				//Delete the user from the group
				g2uclient.applyDelta(group,user,updater)

				//Delete the group from the user
				u2gclient.applyDelta(user,group,updater2)
				
			}
		}
		
		def groupsForUser(user: String): List[String] = {
			
			u2gclient getValue_?(user) match {
				case Some(groups) => groups
				case None => List()
			}
			
		}
		
		def deleteUserFromAllGroups(user: String) {
			
			val groups = groupsForUser(user)
			groups.foreach(group => deleteUserFromGroup(group,user))
			//1 Get the shares for this group
			//2 Per share: see if the user has access 
			//YES: delete share/permissions
			//NO: do nothing
		}
		
		def userInGroup(group:String,user:String): Boolean = {
			
			g2uclient.getValue_?(group) match {
				
				case Some(members) => {
					members.contains(user)
				}
				
				case None => false
				
			}
		}
		
		def userExists(user: String):Boolean = {
			u2gclient.getValue_?(user) match {
				//We use Option and have a value: So, yes....
				case s: Some[List[String]] => true			
				//or no.....
				case None => false
			}
		}
		
		def addUser(user: String) {
			if (! userExists(user)) {u2gclient init(user,List())}
		}
	}	
	
	class VMGroupTest extends VMGroupsAndUsers
}