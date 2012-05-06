import net.vrijheid.clouddrive.pipes._
import net.vrijheid.clouddrive.sharing._
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.httpsupport._
import scala.xml._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.providers.{MetaData}
import java.io.{Serializable}


package net.vrijheid.clouddrive.pipes.webdavcmds {
	
	//
	class PROPPATCHSink[T](implicit val ctx: RootContext[T])  extends Sharing[T] with PipeSink  with Serializable {
		
		private val extras = List("metadata","locks","versions","crypto","original","putlength")
		
		private var header = ""
		private var response = ""
		private var xmlbody = Array[Byte]()
		private var path = ""
		private val protocol = (Config("protocol")).trim;
		
		def processRemove(data: NodeSeq) {
			
			debug("XML data in processRemove = "+data)
			if (data.length > 0) {
				val propertydata = data(0).descendant.iterator.filter(
					x => { x match {
						case n: Elem => true
						case _  => false
					}}
				)			
				propertydata foreach {
					(node) => {
						debug("processing removing of properties")
						//Get the child node for "prop", trim beforehand
						//child(0) is just the first and only Node element
						val propval = node.text
						debug("propval = " + propval)
						//Collect namespace stuff for this node
						val namespace = (node scope)
						debug("namespace = " + namespace)
						val uri = (namespace uri match {
							case s : String => {s}
							case _  => {dav_namespace_uri}
						})
						val prefix = (namespace prefix match {
							case s : String => {s}
							case _  => {dav_namespace_abbrev}
						})
						//Get the property name
						val propname = encodeNamespace("xmlns:" + prefix + "=\"" + stripQuotes(uri) + "\"",(node.label));
						//This will be a tuple (namespace,tagname)
						val propoutput = decodeNamespace(propname)
						debug("propname =" + propname)
						debug("propoutput ns, tag = " + (propoutput _1) + (propoutput _2))
						//Delete the property value
						deleteMetaData(path,propname)
						//Add XML now to the response
						val fragment = "<" + dav_namespace_abbrev  + ":propstat" + " "+ (propoutput _1) +" >" +
						"<" + dav_namespace_abbrev + ":prop>" + 
						"<" + prefix + ":" + (propoutput _2)  + "/>" +  
						"<" + dav_namespace_abbrev + ":status>HTTP/1.1 200 OK </" + dav_namespace_abbrev + ":status>" +
						"</" + dav_namespace_abbrev + ":prop>" +  "</" + dav_namespace_abbrev + ":propstat>"
						response += fragment
					}
				}
			}
		}
		
		def processSet(data: NodeSeq) {
			debug("XML data in processSet = "+data)
			if (data.length > 0 ) {
				val propertydata = data(0).descendant.iterator.filter(
					x => { x match {
						case n: Elem => true
						case _  => false
					}}
				) 
				propertydata foreach {
					(node) => {
						debug("processing setting of properties for node: "+ node)
						//Get the property value
						val propval = node.text
						debug("propval = " + propval)
						//Collect namespace stuff for this node
						val namespace = (node scope)
						debug("namespace = " + namespace)
						val uri = (namespace uri match {
							case s : String => {s}
							case _  => {dav_namespace_uri}
						})
						val prefix = (namespace prefix match {
							case s : String => {s}
							case _  => {dav_namespace_abbrev}
						})
						//Get the property name
						val propname = encodeNamespace("xmlns:" + prefix + "=\"" + stripQuotes(uri) + "\"",(node.label));
						//This will be a tuple (namespace,tagname)
						val propoutput = decodeNamespace(propname)
						debug("propname =" + propname)
						debug("propoutput ns, tag = " + (propoutput _1) + (propoutput _2))
						setMetaData(path,propname,propval)
						//Add XML now to the response
						val fragment = "<" + dav_namespace_abbrev  + ":propstat" + " "+ (propoutput _1) +" >" + 
						"<" + dav_namespace_abbrev + ":prop>" + 
						"<" + prefix + ":" + (propoutput _2)  + "/>" + 
						"<" + dav_namespace_abbrev + ":status>HTTP/1.1 200 OK </" + dav_namespace_abbrev + ":status>" +
						"</" + dav_namespace_abbrev + ":prop>" +  "</" + dav_namespace_abbrev + ":propstat>"
						debug("In processSet response = "+ fragment)
						response += fragment
					}
				}
			}
			
		}
		
		override def ||() {
			
			val fullpath = stripTrailingSlash( "/" + ctx.user  + ctx.verb.header("resource"))
			
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
			
			debug("PROPPATCH sink, entering >|")
			//Some initialization
			val user = "/" + ctx.user
			val hostname = protocol + Config("hostname","localhost")	
		    val resource = ctx.verb.header.getOrElse("resource","/dev/null")
		 	path = stripTrailingSlash(user + resource)			
			val existance = exists(path)
			//Does the resource exist at all
			if (!(existance)) {
				debug("PROPFIND >|: Resource does not exist")
				header = HTTPServerHelper.httpHeader(404,"text/xml; charset=utf-8",0)
			}
			else {
				//Now add data to the XML body
				xmlbody = xmlbody ++ data
			}
					
		}	
		
		override def |>|(data: Array[Byte]) = {
			debug("PROPPATCHSink, enetring |>|")
			if (! xmlbody.isEmpty) {
				//Process the new properties being set
				val xmled = XML loadString(new String(xmlbody))
				debug("XML handed to us: " + xmled)
				processSet(xmled \\ "set" \\ "prop")
				//Process the properties being removed
				processRemove(xmled \\ "remove")
				//Our final response
				response = "<?xml version=\"1.0\" encoding=\"utf-8\" ?><" + dav_namespace_abbrev + ":multistatus " + dav_namespace + ">" + "<" + dav_namespace_abbrev + ":response>" + "<" + dav_namespace_abbrev + ":href>" + ctx.verb.header.getOrElse("resource","/") + "</" + dav_namespace_abbrev + ":href>" + response + "</" + dav_namespace_abbrev + ":response>"  + "</" + dav_namespace_abbrev + ":multistatus>"
				//....and the header to go
				debug(response)
				//Set the header
				header =  HTTPServerHelper.httpHeader(200,"text/xml; charset=utf-8",response)
			}
			data
			
		}	
		
		override def <|(): Array[Byte] = {
			ctx.phase = 'allwritten
			debug(header)
			debug(response)
			(header ++ response) getBytes		
			
		}
		
	}
}