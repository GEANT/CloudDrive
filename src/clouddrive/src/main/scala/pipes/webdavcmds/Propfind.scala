import net.vrijheid.clouddrive.pipes._
import net.vrijheid.clouddrive.sharing._
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.httpsupport._
import scala.xml._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.providers._
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes.webdavcmds {
	
	//
	class PROPFINDSink[T](implicit ctx: RootContext[T]) extends Sharing[T] with PipeSink  with Serializable {
		
		private val extras = List("metadata","locks","versions","crypto","original","putlength")
		
		private var header = ""
		private var response = ""
		private val protocol = (Config("protocol")).trim;
		
		//page 123 etc.
		//propname - return all property names
		//allprops - all supported props on a subset of resources
		//specific properties (note default supported)
		//Depth 0,1, infinity - inifinity is not allowed by Apache
		
		private def closedTag(tag : String) = { "<" + tag + "/>"}
		
		private def allPropsResponseFragment(resource : String, props : List[String]) = {
			debug("in allPropsResponseFragment")
			//HERE WE CREATE THE RESPONSE FOR ALLPROPS <response> ...
			//This will work for default and custom namespaces
			var fragment = "<" + dav_namespace_abbrev + ":response><" + dav_namespace_abbrev + ":href>" + resource + "</" + dav_namespace_abbrev + ":href>" + "<" + dav_namespace_abbrev + ":propstat><" + dav_namespace_abbrev + ":prop>"
			//Rewrite to use namespace with encoding, from the props list
			props foreach { 
				(x) => 
				{
					debug("property in allPropsResponseFragment ="  + x)
					//Extract the namespace abbreviation, uri and construct the tag string:
					// abbreviation:tagname xmlns:abbreviation="uri"
					val (nskey,key) = decodeNamespace(x)
					debug("nskey, key = " + nskey + "," + key)
					val ns = extractNamespace(nskey)
					val ns_uri = extractNamespaceURI(nskey)
					val tagval = ns + ":" + key + " xmlns:" + ns + "=\"" + stripQuotes(ns_uri) + "\""
					fragment = fragment + closedTag(tagval)
				}
			}
			fragment = fragment + "</" + dav_namespace_abbrev + ":prop><" + dav_namespace_abbrev + ":status>HTTP/1.1 200 OK</" + dav_namespace_abbrev + ":status></" + dav_namespace_abbrev + ":propstat></" + dav_namespace_abbrev + ":response>"
			debug(fragment)
			fragment
		}
		
		private def allProps(resources: List[String]) {
			
			debug("in allProps")
			val user = "/" + ctx.user
			val hostname = protocol + Config("hostname","10.0.1.4")
			debug(hostname)
			//body placeholder
			var xmlbody = ""
			//Add the requested info as xml strins per resource
			resources foreach { (resource) =>
				val key = followLink(user + resource)
				val metadata = ((getMetaData(key)) keys).toList;
				xmlbody = xmlbody + allPropsResponseFragment(hostname + resource,metadata)
			}
			//Our final response
			response = "<?xml version=\"1.0\" encoding=\"utf-8\" ?><" + dav_namespace_abbrev + ":multistatus " + dav_namespace + ">" + xmlbody + "</" + dav_namespace_abbrev + ":multistatus>"
			//....and the header to go
			debug(response)
			header = HTTPServerHelper.httpHeader(207,"text/xml; charset=utf-8",response)
			
		}
		
		private def somePropsResponseFragment(resource : String,metadata : Map[String,String],requested_props : List[Node]) : String = {
			debug("in somePropsResponseFragment")
			
			//The idea is that we compare on namespace URIs (fully qualified).
			//So first, "transcode" the available metadata to a key-value map of fully-qualified URI -> metadata
			val new_metadata = (
				for (prop <- (metadata)) yield {
					val prop1 = prop _1 
					val idx = 1 + findIndexOf((prop1),"=")
					val ndx = stripQuotes(prop1.slice(idx,(prop _1) length))
					(ndx, prop _2)
				}
			) toMap;
			
			//Now , create a list of just the fully qualified propnames that are available. We can use this to do set arithmetic 
			//and see what we support for this request
			val available_props = (new_metadata keys) toList
			
			//CREATE NEW REQUESTED PROPS AS WELL WITH FQ NS URI AS KEY
			//These are placed outside the loop for efficiiency reasons (memory)
			var prefix : String = dav_namespace_abbrev
			var uri : String = dav_namespace_uri

			val new_requested_props = (
				for (node <- requested_props) yield {
					//Create the rewritten keyn to match the vailable props

					val scope = node.scope
					node.prefix match {
						case s : String => { prefix = s; uri = scope getURI(prefix)}
						case null => {prefix = dav_namespace_abbrev; uri = scope.uri}
					}

					val nxc = stripQuotes(encodeNamespace(uri,node.label))
					//Put it in a tuple with prop _2, the NamespaceBinding
					(nxc,node)
				}
			//Finally, convert it to a map	
			) toMap;
			
			//We draw the requested properties with their namespace, like above, and create a list so we can do the set arithmetic
			val requested_prop_names  = (new_requested_props keys) toList;
			val unavailable_props = requested_prop_names filterNot (available_props contains)
			val unrequired_props = available_props filterNot (requested_prop_names contains)
			val retrieved_props = ((available_props filterNot (unavailable_props contains)) filterNot (unrequired_props contains))
			//Default namespace
			val dav_namespace_binding = new NamespaceBinding(dav_namespace_abbrev,dav_namespace_uri + ":",TopScope)
			
			debug("requested_props")
			debug(requested_props)
			debug("available_props")
			debug(available_props)
			debug("requested_prop_names")
			debug(requested_prop_names)
			debug("unavailable_props")
			debug(unavailable_props)
			debug("unrequired_props")
			debug(unrequired_props)
			debug("retrieved_props")
			debug(retrieved_props)
			
			//HERE WE CREATE THE RESPONSE FOR ALLPROPS D: (required DAV props) <response> and others that ARE available
			//using NamespaceBinding
			var fragment = "<" + dav_namespace_abbrev + ":response><" + dav_namespace_abbrev + ":href>" + resource + "</" + dav_namespace_abbrev + ":href>" + "<" + dav_namespace_abbrev + ":propstat><" + dav_namespace_abbrev + ":prop>"
			//First, the available ones
			retrieved_props foreach { (property) => {
					//Get namespace, or default to dav_namespace
					val ns = new_requested_props get(property)
					debug("namespace")					
					ns match {
						
						case o : Option[Node] => {
							val node = o.get
							node.prefix match {
								case s : String => { prefix = s; uri = node.scope getURI(prefix)}
								case null => {prefix = dav_namespace_abbrev; uri = node.scope.uri}
							}
							
						}
						
						case _  => {
							prefix = dav_namespace_abbrev
							uri = dav_namespace_uri
						}
						
					}
					debug(uri)
					//Open tag with namespace
					fragment += "<" + prefix + ":" + ((decodeNamespace(property)) _2) + " xmlns:" + prefix + "=\"" + stripQuotes(uri) + "\""  + ">"
					//Content
					fragment += new_metadata(property) 
					//Close
					fragment += ("</" + prefix + ":" + ((decodeNamespace(property)) _2) + ">")
				}
			}
			
			fragment += "</" + dav_namespace_abbrev + ":prop><" + dav_namespace_abbrev + ":status>HTTP/1.1 200 OK</" + dav_namespace_abbrev + ":status></" + dav_namespace_abbrev + ":propstat>"
			debug(fragment)
			//This may be a foreign namespace/scope. Add.
			unavailable_props foreach { (property) => {
				//Get namespace, or default to dav_namespace
				val ns = new_requested_props get(property)
				
					
				ns match {
					
					case o : Option[Node] => {
						val node = o.get
						node.prefix match {
							case s : String => { prefix = s; uri = node.scope getURI(prefix)}
							case null => {prefix = dav_namespace_abbrev; uri = node.scope.uri}
						}
						
					}
					
					case _  => {
						prefix = dav_namespace_abbrev
						uri = dav_namespace_uri
					}
					
				}				
				<D:propstat><D:prop><D:getcontentlength xmlns:D="DAV:"/><getcontentlength/></D:prop><D:status>HTTP/1.1 404 Not Found</D:status></D:propstat>
				//Open response
				fragment += "<" + dav_namespace_abbrev + ":propstat><" + dav_namespace_abbrev + ":prop>"
				//Closed tag on unavilable property
				fragment += "<" + prefix + ":" + ((decodeNamespace(property)) _2) + " xmlns:" + prefix + "=\"" + uri + "\""  + ">"
				//And close it ith a 404 in the DAV namespace
				fragment += "</" + prefix + ":" + ((decodeNamespace(property)) _2) + ">"
				//..the 404.
				fragment +=  "</" + dav_namespace_abbrev + ":prop><" + dav_namespace_abbrev + ":status>HTTP/1.1 404 Not Found</" + dav_namespace_abbrev + ":status></" + dav_namespace_abbrev + ":propstat>"
				}
			}
			debug(fragment)
			fragment + "</" + dav_namespace_abbrev + ":response>"
		}
		
		private def someProps(resources: List[String],body : Elem) {
			
			//Resources should start with username as path! 
			debug("in someProps")
			debug("resources: " + resources)
			val user = "/" + ctx.user
			//We need the hostname (this could be a load balancer as well) for the response
			val hostname = protocol + Config("hostname","10.0.1.4")
			//body placeholder
			var xmlbody = ""
			debug(user)
			debug(hostname)
			//Extract the requested props from the XML as List[String]
			val inside = (body \\"prop") toList;
			debug("inside: " )
			debug(inside)
			debug(" inside(0): " + inside(0))
			debug("inside(0).child: " + inside(0).child)
			//add Namespace binding to form a tuple from x,filtering out PCDATA
			val all_prop_data = ((inside(0).child.filter( x => ((x.label) != "#PCDATA")))) toList;
			debug(all_prop_data)
			//Add the requested info as xml strings per resource
			//Effectively, we loop over the tuples/resources we just generated an create a suitable response fragment per resource
			resources foreach { (resource) =>	
				val key = followLink(user + resource)
				debug("In PROPFIND, key after followlink for + resource = " + key)
				val metadata = getMetaData(key)
				debug("metadata for resource " + resource + " = " + metadata)
				xmlbody = xmlbody + somePropsResponseFragment(hostname + resource,metadata,all_prop_data)
			}
			//Our final response
			response = "<?xml version=\"1.0\" encoding=\"utf-8\" ?><" + dav_namespace_abbrev + ":multistatus " + dav_namespace + ">" + xmlbody + "</" + dav_namespace_abbrev + ":multistatus>"
			//....and the header to go
			debug(response)
			header = HTTPServerHelper.httpHeader(207,"text/xml; charset=utf-8",response)			
		}
		
		private def supportedLocks(fullkey: String) = {
			
			isLocked(followLink(fullkey)) match {
				
				case true => {
					//Get the resource
					val resource = protocol + Config("hostname","10.0.1.4") + ctx.verb.header("resource")
					response += """<?xml version="1.0" encoding="utf-8" ?> <D:multistatus xmlns:D="DAV:"><D:response> <D:href>"""
					response += resource + "</D:href>"
					response += """<D:propstat> <D:prop><D:supportedlock><D:lockentry><D:lockscope><D:exclusive/></D:lockscope><D:locktype><D:write/></D:locktype></D:lockentry><D:lockentry><D:lockscope><D:shared/></D:lockscope><D:locktype><D:write/></D:locktype</D:lockentry></D:supportedlock></D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response></D:multistatus>"""
		
					header = HTTPServerHelper.httpHeader(207,"text/xml; charset=utf-8",response)
				}
				
				case false => {header = HTTPServerHelper.httpHeader(404,"text/plain","")}
			}			
			
		}
		
		
		private def lockDiscovery(fullkey: String) = {
			
			val resource = "http://:" + Config("hostname","10.0.1.4") + ctx.verb.header("resource")
			response += """<?xml version="1.0" encoding="utf-8" ?> <D:multistatus xmlns:D="DAV:"><D:response> <D:href>"""
			response += resource + "</D:href>"
			response += "<D:propstat><D:prop><D:lockdiscovery>"
			isLocked(followLink(fullkey)) match {
				//Not locked, do nothing
				case false => {}
				//Locked, add activelock element
				case true => {
					val lockid = (getLock(fullkey)).get;
					val lockdata = getLockData(fullkey,lockid);
					response += "<D:activelock><D:locktype><D:write/></D:locktype><D:lockscope><D:exclusive/></D:lockscope>"
					response += "<D:timeout>" + getLockTime(fullkey,lockid)+ "</D:timeout>"
					response += lockdata
					response += "<D:locktoken><D:href>" + lockid + "</D:href></D:locktoken></D:activelock>"
				}
				
			}
						
			response += "</D:lockdiscovery></D:prop></D:propstat>"
			response += "</D:response></D:multistatus>"
			header = HTTPServerHelper.httpHeader(207,"text/xml; charset=utf-8",response)			
		}
		
		override def ||() {
			
			val fullpath = stripTrailingSlash( "/" + ctx.user  + ctx.verb.header("resource"))
			
			debug("fullpath in PROPFIND allowacces is " + fullpath)
			
			if(!allowedAccess(fullpath,ctx.user,ctx.verb)) {
				debug("GET: opening store, No Access. Generating 403 response.")
				val header = (HTTPServerHelper httpHeader(403,"text/plain","")) getBytes;
				ctx.bypass.write(header)
				ctx.bypass.close()	
				//Necessary to kill the thread
				throw new Exception()
			}		
		}
		
		override def >|(data : Array[Byte]) {
			
			debug("PROPFIND sink, entering >|")
			
			val user = "/" + ctx.user
			val hostname = protocol + Config("hostname","10.0.1.4")
			
			val depth_like = (ctx.verb.header.getOrElse("Depth","1")).trim 
			//Safe guard against any value that's not 0 or 1
			debug("'" + depth_like + "'")
			val depth = depth_like match {
				case "0" => {depth_like toInt}
				case "1" => {depth_like toInt}
				case _ => {0}
			}
			debug("PROPFIND Depth: " + depth)
			
			val resource = ctx.verb.header.getOrElse("resource","/dev/null")
			var path = stripTrailingSlash(user + resource)
			var resources : Map[String,String] = Map(resource -> path)
			debug(resources)
			val existance = exists(path)
			//Does the resource exist at all
			if (!(existance)) {
				debug("PROPFIND >|: Resource does not exist")
				header = HTTPServerHelper.httpHeader(404,"text/xml; charset=utf-8",0)
			} 
			//Yes, now determine depth and add subresources
			//Then, process XML
			else {
				debug("PROPFIND >|: Resource does exist")
				//Determine the resources to query
				depth match {
					case 0 => { resources}
					case 1 => {
						//Get children, remove metadata stuff, expand using map to full resources
						val children = (getChildren(path)) map { x => path + "/" + x}
						debug("Depth 1, children : " + (children toString))
						//Translate resources to MetaData paths
						
						resources = resources ++ (children map { x => {
								//The slice in the ket for the map returns the original expanded resource (we extract the user from the path)
								//the value is just the fulll expanded metadata key
								(x.slice(user.length,x.length),stripTrailingSlash(x))
							}
						})
						resources 
					}
					case _ => {resources}
				}
				
				//We have the resources, now process the XML to determine the action
				
				debug("resources: " + (resources toString))
			
				debug("Now starting processing of body")
				debug("body = " + new String(data) )
				//Process XML or simply all properties depending on the body being empty or containing XML
				(data length) match {
				
					//Empty Body. Return all properties for the resources, or al available property names depending on depth
					case 0 => {
						debug("Empty Body")
						depth match {
							
							//This case just returns the available properties
							//case 0 => {
							//	allProps((resources keys) toList)
							//}
							
							//This case also returns values
							case _ => {	
								var xmlbody = ""
								debug("---Enterng Loop fpr EMpty body, each resource")
								resources foreach { 
									(resource_pair) =>
										debug("resource_pair = "+resource_pair)
										val metadata = getMetaData(resource_pair _2)
										debug("metadata = "+metadata)
										//Create NamespaceBinding using the property value that contains the namespace
										val all_prop_data = (for (metakey <- (metadata keys))
											yield {
												val (ns,label) = decodeNamespace(metakey)
												val uri = extractNamespaceURI(ns)
												val prefix = extractNamespace(ns)
												val ns_binding = new NamespaceBinding(prefix,uri,TopScope)
												Elem(prefix,label,Null,ns_binding)
											}
									) toList;
									debug("Empty Body, in >|, all_prop_data:")
									debug("all_prop_data for " + (metadata keys) + " = " + all_prop_data)
									//Note: resource is the HTTP resource, not the metadata resource!!!
									xmlbody = xmlbody + somePropsResponseFragment(hostname + (resource_pair _1), metadata, all_prop_data)
								}
								//Our final response
								response = "<?xml version=\"1.0\" encoding=\"utf-8\" ?><" + dav_namespace_abbrev + ":multistatus " + dav_namespace + ">" + xmlbody + "</" + dav_namespace_abbrev + ":multistatus>"
								//....and the header to go
								header = HTTPServerHelper.httpHeader(207,"text/xml; charset=utf-8",response)
							}
						}
						

					}
				
					//XML: pattern match to determine action. 
					//The first two simply return all properties
					//The third match is selectively returning properties
					//The last match is the error fallthru
					case _ => {
							debug("XML body")
							val body = XML loadString(new String(data))
			
							debug("XML body in PROPFIND command >| : \n" + body)
							//Now match the XML to determine what to do
							body match {
				
								case <propfind><propname/></propfind> => {allProps((resources keys) toList)}
								case <propfind><allprop/></propfind> => {allProps((resources keys) toList)}
								case <propfind><lockdiscovery/></propfind> => {lockDiscovery(path)}
								case <propfind><supportedlock/></propfind> => {supportedLocks(path)}
								case <propfind>{_*}</propfind> => {someProps((resources keys) toList,body)}
								//catch all for invalid request
								case _ => {header = HTTPServerHelper.httpHeader(404,"text/xml; charset=utf-8",0)}
							}
						}
					}
				}
			
		}
		
		override def <|() :Array[Byte] = {
			ctx.phase = 'allwritten
			debug(header)
			debug(response)
			(header ++ response) getBytes
		}
		
	}
	
	
}