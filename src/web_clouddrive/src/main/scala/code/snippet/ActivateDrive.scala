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
package net.vrijheid.clouddrive.website.code.snippet {
	
  import net.vrijheid.clouddrive.website.code.lib.login._
  import net.liftweb.common.{ Box, Full,Empty }
  import net.liftweb.http.{S, SHtml}
  import net.liftweb.http.js._  
  import net.liftweb.http.js.JsCmds._
  import scala.xml._
  import net.liftweb.util.Helpers._

  import net.vrijheid.clouddrive.config._
  import net.vrijheid.clouddrive.utils._


  	class ActivateDrive extends Treatise {
	
		var pwd1 = "1"
		var pwd2 = "2"
	
		def samePasswords(): Boolean = {
	      pwd1 == pwd2
	    }
	
		def password(xhtml: NodeSeq): NodeSeq = {
			
			bind("pwd",xhtml,
			"f1" -> SHtml.password("",(s: String) => {pwd1 = s}),
			"f2" -> SHtml.password("",(s: String) => {pwd2 = s}),
			"submit" -> SHtml.submitButton(
				() => {
					if (samePasswords) {
						Login activateDrive(pwd1)
						S.redirectTo("/webdrive")
					}
					else {
						S.notice(S ? "Passwords do not match")
					}
				}
				))
		}
		
		
		def username(xhtml: NodeSeq): NodeSeq = {
			bind("user",xhtml,"name" -> Text(Login.getUserEmail.openOr("")) )
		}
	}
}