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
import net.liftweb.common._
import net.liftweb.http._
import net.vrijheid.clouddrive.utils._

package net.vrijheid.clouddrive.website.code.lib.ssplogin {
	
	object AttributeScanner extends Treatise {
		 
		//Get the secondary attributes, ordered by preference
		val secondary_attr = Config("ssp_user_key","ATTR_mail")
		//Turn them into a list, prefixed by ATTR_mail to have at least something. Remove duplicates using distinct
		val secondary_attr_list = ("ATTR_mail" :: secondary_attr.split(",").map(_.trim).toList).distinct
		
		//Actually get the seconddary attribute
		def getSecondaryAttribute(): Box[String] = {
			
			debug("Attribute scanner, secondary_attr = "+secondary_attr)
			debug("Attribute scanner, secondary_attr_list = "+ secondary_attr_list)
			debug("Attribute scanner, http headers "+ S.request.open_!.headers)
			//A list of attributes that are actually found in the HTTP headers
			var found_attrs: List[String] = List()
			
			//Loop. For each attribuye in the secondary_attr_list, see if it is present. If so, add it to the found_attrs list in reverse order
			secondary_attr_list.foreach({ 
				(attribute) => {
					
					debug("Attribute name = "+attribute)
					debug("Attribute http header value: ")
					S.getRequestHeader(attribute) match {
						case Empty => {debug("None")}
						case Full(name) => debug(name); found_attrs = name :: found_attrs
					}
				}
			})
			
			debug("AttributeScanner, found_attrs = "+found_attrs)
			
			//If we find nothing, then we use the primary attribute
			if(found_attrs isEmpty) {
				found_attrs = List(SSPLogin.main_attribute_id)
			}
			//restore correct order (as we cons'ed the list we created)
			found_attrs = found_attrs reverse;
			//The first item is the first found attribute (we assume they are ordered by preference/likelihood)
			debug("from getSecondaryAttribute, returning: " +(found_attrs head))
			Full(found_attrs head)
		}
	}
}