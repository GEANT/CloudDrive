/*
 * Cloud drive website
 * 
 * Copyright (c) 2010-2012, vrijheid.net

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
package net.vrijheid.clouddrive.website.code {
	package lib {
		package vmlogin {
			
			import net.vrijheid.clouddrive.website.code.model.{Invitees,ResetToken,Users}
			import net.vrijheid.clouddrive.utils._
			import net.vrijheid.clouddrive.setup._
			import net.vrijheid.clouddrive.config._
			import net.vrijheid.clouddrive.website.code.lib.login.LoginLike
			import net.liftweb.common._
			import net.liftweb.http._
			import net.liftweb.mapper._
		
			class VMLogin extends Treatise with LoginLike {
				
				object loginStatus extends SessionVar[Boolean](false)
				object user extends SessionVar[Box[String]](Empty)
				val vmclient = VMTalk getAuthnClient;
				
				
				def userExists(username: String): Boolean = {
					vmclient getValue(username) match {
						case null =>  false
						case _ => true
					}
				}
				
				def logMeIn(username: String,password: String): Boolean = {
					val current = vmclient getValue(username)
					//TBD: update vmclient to use Option, then change this
					userExists(username) match {
						//User does not exist, create
						
						case false => {
							S notice(S ? "User "+username+" does not exist.")
							S redirectTo("/index")
						}
						//User exists, check password
						case true => {
							val current = vmclient getValue(username)
							val haha = current getOrElse("ha1","")
							debug("ha1 ins store = " + haha)
							val ha1 = (md5(username + ":" + Config("realm","beneathclouds.com") + ":" + password)) toLowerCase;
							//Check the password
							(ha1 == haha) match {
								case true => {
									loginStatus(true)
									user(Box(username))
									
								}
								
								case false => {
									loginStatus(false)
								}
							}
						}
					}
					loginStatus.is					
				}
				
				def logout() {
					loginStatus(false)
					user(Empty)
					S.session.open_!.destroySession;
				}
				
				def getCurrentUser = {user.is}
				
				def getUserEmail = getCurrentUser
				
				def loggedIn(): Boolean = {
					loginStatus.is
				}
				
				def generateResetToken(who: String): String = {
					val reset_token =  ResetToken.create;
					reset_token.username(who).token(UUID)
					reset_token.save
					reset_token.token
				}
				
				def setRole(username: String,role: String) = {
					val users = Users.findAll(By(Users.username,username))
					users.map { user => {user.role(role); user.save}}
				}
				
				
				def resetTokenExists(token: String): Boolean = {
					val reset_token = ResetToken.findAll(By(ResetToken.token,token))
					reset_token.length match {
						case 0 => false
						case _ => true
					}					
				}
				
				def passwordReset(who: String,new_password: String) = {
					
					//Reset password if user exists
					userExists(who) match {
						//Nope
						case false => {
							S warning (S ? "User does not exist")
							S redirectTo("/index")
						}
						//Yup
						case true => {
							//Get the current Map for this user
							val current = vmclient getValue(who)
							//Compute the new ha1
							val ha1 = (md5(who + ":" + Config("realm") + ":" + new_password)).toLowerCase;
							//Bluntly update
							//TBD: use applyDelta here???
							vmclient put(who,(Map("ha1" -> ha1) ++ current))
						}
					}
				}
				
				def activateDrive(password: String) {
					//NOOP, this happens magicall on signup
				}
				
				def driveEnabled_? = true
					
				def signup(username: String,password: String) {
					if (invited(username)) {
						val current = vmclient get(username)
						//TBD: update vmclient to use Option, then change this
						current match {
							//User does not exist, create
							case null => {
								VMSetup addUser(username,Map())
								VMSetup activateDavDrive(username,password,Map())
								val who = Users.create
								who.username(username).email(username).role("mortal").balance(0).drive_enabled(true)
								who.save;
								S notice (S ? "You may now login")
								S redirectTo("/login")
							}
							case _ => {
								S notice(S ? "User "+username+" already exists.")
								S redirectTo("/login")
							}
						}
					}
					else {
						S.notice(S ? "You need to be invited to sign up.")
						S.redirectTo("/index")
					}
				}
		
				override def finalize() {
					//vmclient.factory.close
				}
			}
		}
	}
}