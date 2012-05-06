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
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.website.code.lib.ssplogin.{SSPLogin}
import net.vrijheid.clouddrive.website.code.lib.vmlogin._
import net.vrijheid.clouddrive.website.code.model._
import net.liftweb.common._
import net.liftweb.mapper._

package net.vrijheid.clouddrive.website.code.lib.login {
	
	trait LoginLike {
		
		def logout()
		def getCurrentUser(): Box[String]
		def getUserEmail: Box[String]
		def loggedIn(): Boolean
		def notLoggedIn = {! (loggedIn())}
		
		def driveEnabled_?(): Boolean
		def activateDrive(password: String)
		
		def getRole(username: String) = {
			//TBD: use find and a Box.
			val who = Users.findAll(By(Users.username,username))
			
			who.length match {
				case 0 => "none"
				case _ => who(0).role.is
			}
			
		}
		
				
		def isAdmin(username: String) = {
			"admin" == (getRole(username))
		}
	
		def invited(who: String): Boolean = {
			val moi = Invitees.findAll(By(Invitees.username,who))
			moi.length match {
				case 0 => { false}
				case _ => { true}
			}
		}

		def invite(who: String,by_whom: String) {
			val invitee = Invitees.create
			invitee.username(who).by_whom(by_whom)
			invitee.save
		}
		
		//These are necessary for local login, not SSP
		def userExists(username: String): Boolean
		def logMeIn(username: String,password: String): Boolean 
		def signup(username: String,password: String)
		def generateResetToken(who: String): String
		def resetTokenExists(token: String): Boolean
		def passwordReset(who: String,new_password: String)
	}
	
	object Login extends LoginLike {
		
		//Console println("Login delegator object made")
		
		val delegate = Config("login_system","vmlogin") match {
			case "ssp" => {
				SSPLogin
			}
				
			case "vmlogin" => new VMLogin
			case _ => new VMLogin
		}
		
		def localLogin : Boolean = {
			delegate match {
				case x: VMLogin => true
				case _ => false
			}
		}
		
		def logout = delegate logout
		def getCurrentUser = delegate getCurrentUser
		def getUserEmail = delegate getUserEmail
		def loggedIn = delegate loggedIn
		def driveEnabled_? = delegate driveEnabled_?
		def activateDrive(password: String) = delegate activateDrive(password)
		def userExists(username: String): Boolean = delegate userExists(username)
		def logMeIn(username: String,password: String): Boolean = delegate logMeIn(username,password)
		def signup(username: String,password: String) { delegate signup(username,password)}
		def generateResetToken(who: String): String = delegate generateResetToken(who)
		def resetTokenExists(token: String): Boolean = delegate resetTokenExists(token)
		def passwordReset(who: String,new_password: String) {delegate passwordReset(who,new_password)}
		
	}
}