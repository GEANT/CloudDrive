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
package net.vrijheid.clouddrive.website.code.comet {

	import net.liftweb._
	import common._
	import http._
	import util._
	import Helpers._
	import net.liftweb.http.js.JsCmds._
	import scala.xml._
	import java.util.Date
	
	object StatusHolder extends SessionVar[Box[(Long, Long)]](Empty)  
	
	class UploadProgress extends CometActor {
	
		override def defaultPrefix = Full("up")
		
		def main_progress = (<span id="message">Comet init</span>)
		
		def render = bind("main" -> main_progress)
		ActorPing.schedule(this, Tick, 200L)

    	/**
        * In order to get progress updating on a session by session basis, we have to
        * embed this function into the current users session and use a SessionVar
        * extension in order to keep a track of where we are too with the upload
        */
        def sessionProgessListener = {
			S.session.open_!.progressListener = Full({
				(p: Long,q: Long,r: Int) => {
					//Console println("in progress listener"+p+"..."+q)
					StatusHolder(Full((p,q)))
					()
				}
			})
		}
		
		override def localSetup() {sessionProgessListener}
		override def lifespan = Full(5 seconds)		
 
		def getUploadProgress() = {
		    val received: Double = StatusHolder.is.map(_._1.toDouble).openOr(0D)
		    val size: Double = StatusHolder.is.map(_._2.toDouble).openOr(0D)
		    val state: String = if(received == size){ "completed" } else { "uploading" }
			val percentage = (Math.floor((received.toDouble / size.toDouble)*100).toString)
			
			state match {
				case "completed" => {
					//Console println("In completed case")
					if (size == 0){<p>{(new Date).toString}</p>} else {<p>Upload completed</p>}
				}
				case "uploading" => {
					<p>Uploading, {percentage} % completed </p>
				}
				
			}
					
		}
	
		
		override def highPriority : PartialFunction[Any, Unit] = { 
			case Tick => {
				partialUpdate(SetHtml("message",getUploadProgress)) 
				// schedule an update in 10 seconds 
				ActorPing.schedule(this, Tick, 2000L)
			}
		}
	}
			
	
}