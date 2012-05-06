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
import java.util.Random
import net.vrijheid.clouddrive.httpsupport._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.control._
import voldemort.versioning._

package net.vrijheid.clouddrive {
	
	abstract class WebDavAuth()  extends Treatise {
		
		def authenticate() : Boolean = {false}
		//shared digest computational funcs go here
		//not "static" but object based, to get a set of functions per Actor via
		//an instance
		def authNpresent(header : Map[String,String]) = {
			
			debug("in authNpresent")
			debug((header contains "Authorization") toString)
			(
				(header contains "Authorization") &&
				((header("Authorization") trim) startsWith("Digest"))
			)
		}
		
		def computeHA1(user : String,realm : String,password : String) : String = {
			(md5(user + ":" + realm  + ":" + password)) toLowerCase
		}
		
		def computeHA2(verb : String, uri: String) = {
			(md5(verb + ":" + uri)) toLowerCase
		}				
	}

	class DigestMemAuth[T](header : Map[String,String])(implicit ctx : RootContext[T]) extends WebDavAuth  {
		
		def checkDigestAuthN(authzHeader : Map[String,String]) : Boolean = {
			
			debug("authZ map in check digest")
			//debug(authzHeader toString)
			val ha1 = computeHA1(authzHeader("user"),authzHeader("realm"),authzHeader("password"))
			val ha2 = computeHA2(authzHeader("verb"),authzHeader("uri"))
			//We hardcode qop auth; no auth-int as the MD5 of the entity body is not feasible for larger uploads
			//If needed, we'll change it. We're running over SSL anyway (eventually)
			var response : String = ha1 + ":" + authzHeader("nonce") + ":" + authzHeader("nc") + ":" + authzHeader("cnonce") + ":" + authzHeader("qop") + ":" + ha2
			response = stripQuotes(response)
			
			debug(authzHeader("response"))
			debug(md5(response) toLowerCase)
			
			(authzHeader("response") ==  (md5(response) toLowerCase))
		}
		
		override def authenticate() : Boolean = {
			
			debug("authn present")
			//debug((authNpresent(header)) toString)
			//First safeguard against config file errors and invalid headers		
			if (
				(authNpresent(header)) &&
				(Config("static_user") != "") &&
				(Config("static_password") != "")
			)
			{
				//Good, let's check the header
				val user = Config("static_user","maarten")
				//Store user in Context
				ctx.user = user
				val password = Config("static_password","passthru")
				//implement authentication check
				//Check the digest
				var authzHeader = ((header("Authorization")) trim)
				//Strip "Digest string"
				authzHeader = authzHeader slice (7, authzHeader length)
				val authZlines = authzHeader split ","
				var authZMap : Map[String,String] = Map("user" -> user,"password" -> password,"verb" -> header("verb"))
				authZlines foreach {
					line =>
					//Extract key value, and eliminate "
					val lline = line split "="
					val key = lline(0).trim()
					var value = lline(1).trim()
					value = value replaceAll("\"","")
					//Add it to the Map
					authZMap = authZMap ++ Map(key -> value) 
				}
				//Do the actual in memory authorization and return that value
				checkDigestAuthN(authZMap)
			} else {false}
		}
	}

	//TBD: Sometime, whenever somebody needs it
	class DigestMySQLAuth(header : Map[String,String]) extends WebDavAuth  {
		
		override def authenticate() : Boolean = {false}
		
	}
	
	//Sometime, whenever somebody needs it
	class DigestVoldemortAuth[T](header : Map[String,String])(implicit ctx : RootContext[T]) extends WebDavAuth {
		
		private def Win7Lockvalidate(): Boolean = {
			
			try {
				val ifheader = ctx.verb.header.getOrElse("If","nothing")
				val token = (ifheader.trim().stripPrefix("(<opaquelocktoken:").stripSuffix(">)"))
				debug("token = "+ token)
				//Only if the toke exists
				val lock2user = new LockToUser
				if (lock2user.hasLock(token)) {
					debug("lock exists")
					lock2user.getUser(token) match {
						case None => false
						case Some(user) => {
							ctx.user = user
							debug("user is "+user)
							true
						}
					}
				}
				else false
			}
			//We had an error when trying to get the token, so we are cautious and return unauthorized
			catch {case _ => false}
		}
		
		override def authenticate() : Boolean = {
			debug("Authenticating in Voldemort")
			if (authNpresent(header)){
				debug("authN header present")
				//Here we inline a bit more due to the ZooKeepr connection
				val vmclient = VMTalk getAuthnClient;
				debug("Voldemort authn client")
				//Get the authZ header
				var authzHeader = ((header("Authorization")) trim)
				debug("authz header: " + authzHeader)
				//Strip "Digest string"
				authzHeader = authzHeader slice (7, authzHeader length)
				val authZlines = authzHeader split ","
				var authZMap = Map ("verb" -> header("verb"))
				debug("authZMap initializd")
				//Build a Map(key->val) for the Authorization header
				authZlines foreach {
					line =>
					//Extract key value, and eliminate "
					val lline = line split "="
					val key = lline(0).trim()
					var value = lline(1).trim()
					value = value replaceAll("\"","")
					//Add it to the Map
					authZMap = authZMap ++ Map(key -> value) 
				}
				debug("authzMap: " + authZMap)
				val user = authZMap getOrElse("username","/noauth")
				debug(user)
				//Store user in Context
				ctx.user = user
				//Match the username			
				user match {
					//Username field not in header, we aren't gonna try
					case "/noauth" => {
						debug("authentication failed")
						false
					}
					//Let's check, for this user...
					case _ => {
							debug("computing authentication")
							//ha1 comes from our Voldemort cluster, the default value prevents crashes/errors when the user is not found						
							val ha1 = (vmclient.get(user,new Versioned(Map("ha1" -> ""))).getValue).getOrElse("ha1","").toLowerCase;
							debug("Computed ha1 = " + ha1)
							//Now, compute ha2 based on the Map key,vals
							val ha2 = md5(authZMap("verb") + ":" + authZMap("uri")) toLowerCase;
							debug("Computed ha2 = " + (ha2) toString)
							//Response basd on Map(key->val) and ha1 and ha2
							var response = ha1 + ":" + authZMap("nonce") + ":" + authZMap("nc") + ":" + authZMap("cnonce") + ":" + authZMap("qop") + ":" + ha2
							debug("authZ response should be: " + response)
							//Maybe redundant but we get bitten in Java too often: make sure quotes are gone
							response = (md5(stripQuotes(response))) toLowerCase();
							//vmclient.factory.close;
							debug("authZ response should be( MD5'ed): " + response)
							//Do the check
							//debug(authZMap("response"))
							//debug(response)
							//debug( "****************************************";
							(authZMap("response") ==  (response))
						}
					}
			}
			//No Authorization header present.... 
			else {
				debug("authN header not present, checking for PUT and lock token")
				//This is is all to handle the case for Win7 explorer.
				//It tries to write ob obtaining a lock WITHOUT authenticating, but
				//WITH sending the lock!!
				ctx.verb match {
					case v: PUT  => {
						debug("Verb is PUT")
						Win7Lockvalidate()
					}
					case v: PROPPATCH  => {
						debug("Verb is PROPPATCH")
						Win7Lockvalidate()
					}
					case v: UNLOCK  => {
						debug("Verb is UNLOCK")
						Win7Lockvalidate()
					}
					case v: DELETE  => {
						debug("Verb is DELETE")
						Win7Lockvalidate()
					}
					//We only check for tokens on PUT without authn
					case _ => false
				}
				
			}			
		}
		
	}	

	
	//This one is used to "always deny"
	class Never() extends WebDavAuth {
		override def authenticate() : Boolean = {false}
	}	
	
	//This one is used to "always allow" for public servers and authN testing with client libs
	class Always[T]()(implicit ctx : RootContext[T])  extends WebDavAuth {
		override def authenticate() : Boolean = {ctx.user = Config("static_user","maarten"); true}
	}

	object WebDavAuth {
		
		def getAuthenticator[T](kind : Symbol, header : Map[String,String])(implicit ctx : RootContext[T]) = synchronized {
						
			val auth_object = kind match {
				
				case 'mysql => new DigestMySQLAuth(header)
				case 'memory => new DigestMemAuth(header)
				case 'static => new DigestMemAuth(header)
				//Legacy
				//case 'zookeeper => new DigestZooAuth(header)
				case 'voldemort => new DigestVoldemortAuth(header)
				case 'always => new Always()
				case _ => new Never()
			}
			//We return a closure over the authenticator instance
			{ () => { auth_object.authenticate() }}
			
		}
	}	
}
