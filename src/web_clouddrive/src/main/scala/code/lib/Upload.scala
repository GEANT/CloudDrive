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
		package files {
			import net.vrijheid.clouddrive.website.code.lib.vmlogin._
			import net.vrijheid.clouddrive.control._
			import net.vrijheid.clouddrive.pipes._
			import net.vrijheid.clouddrive.providers._
			import net.vrijheid.clouddrive.config._
			import net.vrijheid.clouddrive.httpsupport._
	        import net.liftweb.common.{ Box, Full, Empty }
		    import net.liftweb.http._
		    import net.liftweb.util.Helpers._
				
			class UploadManager  {
				
			}
			

			//CODE_CC, clean up or move t HTML5 binary upload?
			
			object LiftUploadManager {

				
				def receiveUpload: LiftRules.DispatchPF = { 
				  case Req("upload" :: Nil, _, PostRequest) => () => {
						Console println("********** We have an upload file request!!!!!")
						val files = S.request.open_!.uploadedFiles
						Console println("***************************" + files)
						Full(new OkResponse)
				  }
				}
			}
		}
	}
}