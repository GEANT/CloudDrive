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
 
package bootstrap.liftweb

import javax.mail._
import javax.mail.internet._

import net.liftweb._
import net.liftweb.util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import mapper._

import net.vrijheid.clouddrive.website.code.model._
import net.vrijheid.clouddrive.website.code.lib.login._
import net.vrijheid.clouddrive.website.code.lib.files._

import net.liftweb.http.{ LiftRules,RewriteRequest, RewriteResponse,ParsePath,Req}
import net.vrijheid.clouddrive.config.{Config}
import net.vrijheid.clouddrive.utils.{Treatise}


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Treatise {
	
	
	
  def configMailer(host: String, user: String, password: String) {
	    // Disable TLS support
	    System.setProperty("mail.smtp.starttls.enable","false");
	    // Set the host name
	    System.setProperty("mail.smtp.host", host) // Enable authentication
	    System.setProperty("mail.smtp.auth", "true") // Provide a means for authentication. Pass it a Can, which can either be Full or Empty
	    Mailer.authenticator = Full(new Authenticator {
	      override def getPasswordAuthentication = new PasswordAuthentication(user, password)
	    })
  }	
	
  def boot {
	
	LiftRules.uriNotFound.prepend(NamedPF("404handler"){
	      case (req,failure) => 
	        NotFoundAsTemplate(ParsePath(List("404"),"html",false,false))
	    })
	
	//Comment this in if we want to use the assembly task, ever.
	//LiftRules.servletAsyncProvider = (req) => new Jetty7AsyncProvider(req)
	
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

	configMailer("smtp.yourhost.net","user@yourhost.net","passwd")

    // Use Lift's Mapper ORM to populate the database
    Schemifier.schemify(true, Schemifier.infoF _, ResetToken)
    Schemifier.schemify(true, Schemifier.infoF _, Invitees)
    Schemifier.schemify(true, Schemifier.infoF _, Users)

	//Enhance some max sizes
	LiftRules.maxMimeFileSize = 6173741824L
	LiftRules.maxMimeSize = 6273741824L
	
    // where to search snippet
    LiftRules.addToPackages("net.vrijheid.clouddrive.website.code")
	//Make sure we don't put stuff in memory for uploads
	LiftRules.handleMimeFile = OnDiskFileParamHolder.apply
 	
	val public_root = (stripLeadingSlash(Config("public_folder_root")).toLowerCase())

	
	LiftRules.statefulRewrite.append {
		//This rewrites anny path of the type /webdrive/path/to/file to /webdrive with a parameter "resource" that has a value "path/to/file"
		case RewriteRequest(ParsePath("webdrive" :: path,_,_,_),_,_) if !path.isEmpty => RewriteResponse("webdrive" :: Nil,Map("resource" -> ("/" + path.mkString("/"))))
	}

    LiftRules.statefulRewrite.append {
		//This rewrites anny path of the type /download/path/to/file to /download with a parameter "resource" that has a value "path/to/file"
		//The suffix stuff is a) to extract the suffix and b) to remove a trailing "."
		case RewriteRequest(ParsePath("download" :: path,suffix,_,_),_,_) if !path.isEmpty => RewriteResponse("download" :: Nil,Map("resource" -> ("/" + path.mkString("/") +"."+suffix)))
	}
	
	LiftRules.statefulRewrite.append {
		//This rewrites anny path of the type /download/path/to/file to /download with a parameter "resource" that has a value "path/to/file"
		//The suffix stuff is a) to extract the suffix and b) to remove a trailing "."
		case RewriteRequest(ParsePath("public" :: user:: path,suffix,_,_),_,_) if !path.isEmpty => RewriteResponse("public" :: Nil,Map("resource" -> ("/" + path.mkString("/") +"."+suffix),"user" -> user))
	}
	
	


	val protected_page = {() => {
		Login.loggedIn() match {
			case true => Empty
			case false => {
				//S.notice (S ? "You must be logged in to access this page")
				Full(RedirectResponse("/login"))
			}
		}
	}}
	
	val admin_page = {
		() => {
			Login.isAdmin(Login.getCurrentUser.openOr("")) match {
				case true => Empty
				case false => {
					Full(RedirectResponse("/webdrive"))
				}
			}
		}
	}

	//This ensures session on DispatchPFs
	val ensureSession: PartialFunction[Req, Unit] = {
		case _ if Login.loggedIn =>
	}	
	
	LiftRules.dispatch.append(LiftDownloadManager.publicDownload)
	LiftRules.dispatch.append(ensureSession guard LiftDownloadManager.serveDownload)
	LiftRules.dispatch.append(ensureSession guard LiftUploadManager.receiveUpload)
	
	//Allow images from the ResourceServer
	ResourceServer.allow { 
		case "images" :: _ => true 
		case "css" :: _ => true
	}
	
    // Build SiteMap
    val entries = List(
      // the simple way to declare a menu
      Menu.i("Home") / "index" >> If(Login.notLoggedIn _,() => RedirectResponse("/webdrive")), 
	  Menu("Login") / "login" >> If(Login.localLogin _,RedirectResponse("/index")) >> If(Login.notLoggedIn _,() => RedirectResponse("/index")),
	  Menu("Sign up") / "signup" >> If(Login.localLogin _,RedirectResponse("/index")) >> If(Login.notLoggedIn _,RedirectResponse("/index")),
	  Menu("Lost password") / "lost_password" >> If(Login.localLogin _,RedirectResponse("/index")) >> If(Login.notLoggedIn _,RedirectResponse("/login")),
	  Menu("Logout") / "logout" >> If(Login.loggedIn _,RedirectResponse("/index")) >> EarlyResponse(() => {
		Login.logout()
		S.notice("Logged out.")
		Full(RedirectResponse("/index"))
	  }),
	  
	  Menu("Reset password") / "reset_password" >> If(Login.localLogin _,RedirectResponse("/index")) >> Hidden ,
	  Menu("Search results") / "search" >> Hidden >> TestAccess(protected_page),
	  Menu("Activate DavDisk") / "activate_drive" >>  Unless(Login.localLogin _,RedirectResponse("/index")) >> TestAccess(protected_page),
	  Menu("Webdrive") / "webdrive" >> TestAccess(protected_page),
	  Menu("Invite") / "add-invite" >> If(Login.localLogin _,RedirectResponse("/index")) >> TestAccess(protected_page) >> TestAccess(admin_page),
      // more complex because this menu allows anything in the
      // /static path to be visible
      Menu(Loc("Help", Link(List("help"), true, "/help/index"), 
      //Menu(Loc("Help", Link(List("https://ow.feide.no/metacenter:cloud:storage"), true, "https://ow.feide.no/metacenter:cloud:storage"), 
	       "Help Content")))
	
    // the User management menu items
    //User.sitemap

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMap(SiteMap(entries:_*))

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    //LiftRules.loggedInTest = Full(() => User.loggedIn_?)

	LiftRules.resourceNames = "Gumbah" :: Nil

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)
  }
}
