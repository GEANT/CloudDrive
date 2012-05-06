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
		
		protected def deleteGroup (group: String) 

		def addGuestUser(user: String,config: Map[String,String]) 
		
		def deleteGuestUser(user: String) 
		
		protected def addUsertoGroup(group: String,user: String) 
				
		protected def deleteUserFromGroup(group: String,user: String) 
		
		def groupsForUser(user: String): List[String] 
		
		protected def deleteUserFromAllGroups(user: String) 
		
		def userInGroup(group:String,user:String): Boolean 
		
		def userExists(user: String):Boolean 
		
		def addUser(user: String)
	}

	
	trait VMGroupsAndUsers extends GroupsAndUsers {
		
		//Get our group client, and the reverse mapping
		val u2gclient = VMTalk.getUserToGroupClient
		val g2uclient = VMTalk.getGroupClient
		val guest_client = VMTalk.getGuestClient
		
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
		
		protected def deleteGroup (group: String) {
			
			//FIrst who, is still a member
			val members = g2uclient getValue group;
			//Remove this group for these users from their groups list
			//This also removes the groups from the users list via deleteUserFromGroup
			members.foreach(member => deleteUserFromGroup(group,member))
			//Now, delete the group (effectively empty)
			g2uclient delete group;			
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
		
		protected def addUsertoGroup(group: String,user: String) {
			
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
			}			
		}
				
		protected def deleteUserFromGroup(group: String,user: String) {
			
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
			
			if(userInGroup(group,user)) {

				//Delete the user from the group
				g2uclient.applyDelta(group,user,updater)

				//Delete the group from the user
				u2gclient.applyDelta(user,group,updater2)
				
			}
			
			//CODE CC: remove all shares here as well for the groups of this user???
			//Couples more tightly between group and share management.
			// Traverse the "shares" tree for the user and remove "dead" symlinks based on groups ACL
			// Expensive, but will work. Other option: add a group2shares store.
			
			//Other option: this is mixed in with VoldemortSharing - do an override on this method or a slight variation.
		}
		
		def groupsForUser(user: String): List[String] = {
			
			u2gclient getValue_?(user) match {
				case Some(groups) => groups
				case None => List()
			}
			
		}
		
		protected def deleteUserFromAllGroups(user: String) {
			
			val groups = groupsForUser(user)
			groups.foreach(group => deleteUserFromGroup(group,user))
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