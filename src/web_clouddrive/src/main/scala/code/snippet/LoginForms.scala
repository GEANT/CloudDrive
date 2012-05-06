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
	
  import net.liftweb.mapper.MappedEmail
  import net.vrijheid.clouddrive.website.code.lib.login._
  import net.vrijheid.clouddrive.config._
  import net.liftweb.common.{ Box, Full }
  import net.liftweb.http.{ LiftScreen, S, SHtml }
  import net.liftweb.util.Mailer._
  import scala.xml._


  object LogMeIn extends LiftScreen {

    val username = field(S ? "email", "")
    val pwd = password(S ? "password", "")
    val req_password = link("/lost_password",()=>{},Text(S ? "Lost password"))
    val signup = link("/signup",() => {},Text(S ? "...for the good stuff, here"))

    override def validations = loggedIn _ :: super.validations

    def loggedIn(): Errors = {
      if (!(Login.logMeIn(username.is, pwd.is))) { "Login failed." }
      else Nil
    }

    def finish() = {
      // print out number.is, text.is, etc.
      S.redirectTo("/webdrive")
    }
  }

  object SignUp extends LiftScreen {

    val username = field(S ? "email", "")
    val pwd = password(S ? "password", "")
    val pwd2 = password(S ? "password (again)", "")

    //Validate passwords
    override def validations = validEmail _ :: invited _ :: samePasswords _ :: super.validations

    def samePasswords(): Errors = {
      if (!(pwd.is == pwd2.is)) { S ? "Passwords do not match" }
      else Nil
    }

    def invited(): Errors = {
      //check for invites
      if (!Login.invited(username.is)) {S? "Party crasher. Please don't even try anymore!" }
      else Nil
    }

    def validEmail(): Errors = {
      if (!MappedEmail.validEmailAddr_?(username.is)) { S ? "That's not a valid email address." }
      else Nil
    }

    def finish() = {
      Login.signup(username.is, pwd.is)
      //TBD add email activation; but as our main login is via ssp and this if for local setups only (dev), low prio
      S.notice(S ? "Please login now...")
      S.redirectTo("/login")
    }
  }

  object LostPassword extends LiftScreen {

    val username = field(S ? "email", "")

    def userExists(): Errors = {
      if (!(Login.userExists(username.is))) { "Username not known...." }
      else Nil
    }

    override def validations = userExists _ :: super.validations

    def finish() = {
      val reset_token = Login.generateResetToken(username.is)
      //send email with token
      sendMail(From(Config("send_email_from","nobody@localhost")), Subject("You're reset token for Cloud backed storage password restore"), To(username.is), PlainMailBodyType(
	 "You're password reset token: " + reset_token + "\n" + "Please go to "+Config("protocol") + Config("hostname") + "/reset_password" +"\n\n" + "Here you'll be able to reset your password by pasting in the provided code."))
    }
  }

  object ResetPassword extends LiftScreen {

    val username = field(S ? "email", "")
    val reset_token = field(S ? "Type your reset code here", "")
    val pwd = password(S ? "password", "")
    val pwd2 = password(S ? "password (again)", "")

    //Validate passwords etc,
    override def validations = resetTokenExists _ :: userExists _ :: samePasswords _ :: super.validations

    def resetTokenExists: Errors = {
      if (!Login.resetTokenExists(reset_token.is)) { S ? "Invalied rest token." }
      else Nil
    }

    def samePasswords(): Errors = {
      if (!(pwd.is == pwd2.is)) { S ? "Passwords do not match" }
      else Nil
    }

    def userExists(): Errors = {
      if (!(Login.userExists(username.is))) { "Username not known...." }
      else Nil
    }

    def finish() = {
      Login.passwordReset(username.is, pwd.is)
      Login.logMeIn(username.is, pwd.is)
      S.redirectTo("/webdrive")
    }
  }

  object AddInvite extends LiftScreen {

    val username = field(S ? "Email address invitee", "")

    override def validations = validEmail _ :: super.validations

    def validEmail(): Errors = {
      if (!MappedEmail.validEmailAddr_?(username.is)) { S ? "That's not a valid email address." }
      else Nil
    }

    def finish() = {
	  val invite_text = "Welcome! \n\nYou're invited to a cloud backed storage private beta with 10GB of online storage. Beware that this is a private beta, and things can (and will) go wrong. Don't rely on it. The private beta will end around September 1st.\n\nGo to "+ Config("protocol") + Config("hostname") +   " and sign up using this email address by clicking on 'Sign up'. \n\nYou can then connect via the website. The website will instruct you how to connect to the disk interface once you've accepted the invitation - READ THE HELP section. \n\n Basically, any WebDAV client will do for a network disk if you connect it to the URL "+ Config("protocol") + Config("hostname") +  ":444 \n\nIn the tesing phase we are not always using officially validated certificates, so you may get a warning. Rest assured, it's safe to accept them for now - they merely enable the safe SSL basedtransport to and from your computer\n\n" + "Finally, a brief supported client list: Windows 7, OS X Finder, OS X Transmit and Cyberduck, iOS Goodreader, WebDavNav and iWork, Android WebDavNav Pro and Lite and Linux davfs2 with the if_match_bug set to 1 in the config file.\\n"+"These clients will all gve you a disk interface once you have signed up.\n\n"+"Enjoy, and give feedback to storage@vrijheid.net if you have any!\n\n Enjoy, The RightFabric team."
      Login.invite(username.is, Login.getUserEmail.openOr(""))
      sendMail(From(Config("send_email_from")), Subject("You're invited to Cloud backed storage"), To(username.is), PlainMailBodyType(invite_text))
    }
  }

}
