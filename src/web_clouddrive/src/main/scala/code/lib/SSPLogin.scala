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
package net.vrijheid.clouddrive.website.code.lib.ssplogin {
	
	import net.liftweb.common._
	import net.liftweb.http._
	import net.liftweb.mapper._
	import net.vrijheid.clouddrive.website.code.lib.login.LoginLike
	import net.vrijheid.clouddrive.setup._
	import net.vrijheid.clouddrive.config._
	import net.vrijheid.clouddrive.utils._
	import net.vrijheid.clouddrive.website.code.model.{Invitees,ResetToken,Users}
	
	
	
	object loginStatus extends SessionVar[Boolean](false)
	object username extends SessionVar[Box[String]](Empty)
	object user_email extends SessionVar[Box[String]](Empty)
	object drive_enabled extends SessionVar[Box[Boolean]](Empty)
	
	
	trait WhiteList extends Treatise {
		
		var privileged_filename = "unknown"
		var use_whitelist = false
		var privileged: List[String] = List()
		
		//Check for option to enable whitelisting in order to execut block below
		use_whitelist = (Config("ssp_whitelisting","true") == "true")
		
		if (use_whitelist) {
			debug("Using whitelist")
			//Check for option for the privileged filename
			privileged_filename = Config("ssp_whitelist_file","unknown")
			debug("whitelist file = "+privileged_filename)
			if(! (privileged_filename == "unknown")) {
				val props = new java.util.Properties;
				props.load(new java.io.FileInputStream(privileged_filename))
				debug("whitelist loaded")
				privileged = props.getProperty("privileged") match {
					//Sadly, this may return null. Basic protection. We return a List[String] directly, an empty List otherwise
					case x: String => x.split(",").map(_.trim).toList
					case _ => List()
				}

				debug("Lucky users are: " + privileged)
			}
		}
		
		
		//Check for option to enable whitelisting etc
		def onWhitelist(who: String) = {
			//Check to see that we're running in whitelist mode and we have loaded or whitelist file
			if(use_whitelist && !(privileged_filename == "unknown")) {
				privileged.contains(who)
			} else {false}
		}
		
	}
	
	//We can add utility methods here, currently a placeholder.
	//trait SSPLogin
	
	//object SSPLogin extends SSPLogin with LoginLike with WhiteList {
	object SSPLogin extends LoginLike with WhiteList {
		
		//Set the main_attribute_id for SSP, defaulting to ATTR_eduPersonPrincipalName if none present
		val main_attribute_id = Config("ssp_main_attribute","ATTR_eduPersonPrincipalName")
		
		debug("In SSPLogin, main_attribute_id = "+ main_attribute_id)
				
		def loggedIn = {
			debug("In SSP, loggedIn")
			//Console println("In loggedIn, headers are:" + S.request.open_!.headers)
			//try and login
			if(!loginStatus.is) {
				//Console println("inside if, loginStatus.is == true")
				//Try and get the values from the Identity provider
				//Fetch the main attribute for SSP, or eduPersonPrincipalName if none configured (see above)
				//This ends up as the "username" SessionVar/database field (and will be unique)
				val main_attribute = S.getRequestHeader(main_attribute_id)
				debug("main_attribute from http header value is "+main_attribute)
				//TBD: make mail configurable via the attribute scanner (to be written)
				//This ends up as the user_email SessionVar and "email" database field, and should be a unique hint for a username
				val email: Box[String] = AttributeScanner getSecondaryAttribute;
				debug("email (secondary attribute value from http header) = "+email)
				//Extract and match, set status to true if match on both succesfull
				(main_attribute,email) match {
					case (Full(main),Full(mail)) => {
						if(! onWhitelist(main)) {false}
						else {
							username(Full(main))
							user_email(Full(mail))
							ensureUser(main,mail) 
							loginStatus(true)
						}
					}
					
					//TBD: add case for mail unknown (not set as attribute)
					
					case _ => {
						//Console println ("no match, not logged in")
						loginStatus(false)
					}
				}
			}
			loginStatus.is 
		}
		
		def ensureUser(main_attribute: String,mail: String) {
			Users.find(By(Users.username,main_attribute)) match {
				//exists, do nothing
				case Full(main) => {
					//NOOP
				}
				//Empty, create a Profile
				case Empty => {
					val user = Users.create;
					VMSetup addUser(mail,Map())
					user.username(main_attribute).role("mortal").email(mail).drive_enabled(false).save()
				}
				case x: Failure => {x ?~ (S ? "Something is wrong with the database")}
			}
			
		}
		
		def driveEnabled_? : Boolean = {
			
			drive_enabled.is match {
				//We already have this cached value
				case Full(drive) => drive
				//We don't have a value, one time lookup in the database
				case _ => {
					//Check to se weh do have a username (we should)
					username.is match {
						//Yes
						case Full(s) => {
							//Query the database and extract our user
							Users.find(By(Users.username,s)) match {
								//We have a user
								case Full(user) => {
									//Get the value, set the SessionVar for caching, and return the value
									val able = user.drive_enabled.is
									drive_enabled(Full(able))
									able
								}
								//User not found (should not happem)
								case Empty => false
								//Speaks for itself , connection loss, what have you
								case x: Failure => {x ?~ (S ? "Something is wrong with the database");false}
							}
						}
						//No current user, so we return false by default
						case _ => false
					}
				}
			}
		}
		
		def activateDrive(password: String) {
			
			user_email.is match {
				case Full(s) => {
					VMSetup activateDavDrive(s,password,Map())
				}
				
				case _ => {}
			}
			
		}
		
		def setUser(id: String) {username(Full(id)) }
		def getCurrentUser() = { 
			username.is 
		}
		def setUserEmail(email: String) {user_email(Full(email))}
		def getUserEmail() = {
			user_email.is 
		}
		
		def logout() {
			loginStatus(false)
			S.session.open_!.destroySession
			//TBD - make the logout url configurable
			S.redirectTo("/simplesaml/module.php/core/as_logout.php?AuthId=default-sp&ReturnTo=/index")
		}
		
		//Not applicable in SSP context, so all false/empty method body
		def userExists(username: String): Boolean = false
		def logMeIn(username: String,password: String): Boolean = false
		def signup(username: String,password: String) {}
		def generateResetToken(who: String): String = ""
		def resetTokenExists(token: String): Boolean = false
		def passwordReset(who: String,new_password: String) {}
		
	}
	
}