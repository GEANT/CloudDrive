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
	package model {

		import _root_.net.liftweb.mapper._
		import _root_.net.liftweb.util._
		import _root_.net.liftweb.common._
		import net.vrijheid.clouddrive.setup._
		
		object Invitees extends Invitees with LongKeyedMetaMapper[Invitees] {
			override def dbTableName = "invitees"
		}
		
		class Invitees extends LongKeyedMapper[Invitees] with IdPK {
			
			object username extends MappedString(this,150)
			object by_whom extends MappedString(this,150)
			def getSingleton = Invitees
		}
		
	}
}