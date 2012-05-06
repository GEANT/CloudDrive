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
  import net.vrijheid.clouddrive.website.code.lib.files._
  import net.liftweb.common.{ Box, Full,Empty }
  import net.liftweb.http.{S, SHtml,FileParamHolder, RequestVar }
  import net.liftweb.http.js._  
  import net.liftweb.http.js.JsCmds._
  import net.liftweb.http.js.jquery.JqJsCmds._
  import scala.xml._
  import net.liftweb.util.Helpers._
  import net.vrijheid.clouddrive.utils._
  import net.vrijheid.clouddrive.config._

	object current_tags extends RequestVar[List[String]](List())
	object current_tagged_resources extends RequestVar[List[String]](List())

  	class WebDrive extends Treatise {
	
		
    	val headers = Map("resource" -> S.param("resource").openOr("/"));

		val resource = headers("resource")
		val cloudfiles = new CloudFiles(headers)
		val nullfunc = {() => ()}
		lazy val collections = (cloudfiles.listSubCollections).sortWith((e1, e2) => (e1 compareTo e2) < 0)
	    lazy val resources = (cloudfiles.listSubResources).sortWith((e1, e2) => (e1 compareTo e2) < 0)
	    var fileHolder : Box[FileParamHolder] = Empty
		
		def dirHeader(xhtml:NodeSeq): NodeSeq = {
			if (collections.length > 0) {
				cloudfiles.inTagFolder() match {
					case true => bind("dir",xhtml,"title" -> <b>{S ? "Smart folders"}</b>)
					case false => bind("dir",xhtml,"title" -> <b>{S ? "Dirs"}</b>)
				}
			}
			else bind("dir",xhtml,"title" -> "")
		}
		
		def fileHeader(xhtml:NodeSeq): NodeSeq = {
			if (resources.length > 0) {
				cloudfiles inTagFolder match {
					case true => bind("file",xhtml,"title" -> <b>{S ? "Smart search results:"}</b>)
					case false => bind("file",xhtml,"title" -> <b>{S ? "Files"}</b>)
				}
			}
			else bind("file",xhtml,"title" -> "")
		}	
		
		def breadCrumbs(xhtml: NodeSeq): NodeSeq = {
			val resource_list = resource.split("/")
			if (resource_list.length > 1 ) {
				val resources = resource_list.slice(0,-1 + resource_list.length).map("/" + _).toList
				resources.flatMap(item => bind("bread",xhtml,"crumbs" -> SHtml.link(stripTrailingSlash("/webdrive" + item),nullfunc,Text(item+"  "))))
			} else bind("bread",xhtml,"crumbs" -> "")
		}
		
		def listCollections(xhtml: NodeSeq): NodeSeq = {
			collections.flatMap(coll => {
				
				val coll_path = stripTrailingSlash(resource) + "/" + stripTrailingSlash(coll)
				var current_coll = coll
				
				debug("$$$ isTagFolder in listCollections for "+coll_path+" = "+cloudfiles.isTagFolder_?(coll_path))
				
				cloudfiles.isMandatoryFolder(coll_path) match {
					
					case true => {
						bind("coll",xhtml,
							"item" -> SHtml.link("/webdrive"+stripTrailingSlash(resource)+"/"+coll,nullfunc,Text(coll)),
							"delete" -> <div></div>)
					}
				
					case false 	=> {
						
						cloudfiles.isTagFolder_?(coll_path) match {
							
							case true => {
								bind("coll",xhtml,
									"item" -> SHtml.link("/webdrive"+stripTrailingSlash(resource)+"/"+coll,nullfunc,Text(coll)),
									"delete" -> <div>
										<lift:WebDrive.deleteCollection form ="POST">
										<input type="hidden" name="delpath" value={coll}/>
										<del:submit/>
										</lift:WebDrive.deleteCollection>
										</div>
										)								
							}
						
							case false => {
								bind("coll",xhtml,
									"item" -> SHtml.ajaxEditable(
										SHtml.link("/webdrive"+stripTrailingSlash(resource)+"/"+current_coll,nullfunc,Text(current_coll)),
										SHtml.text(current_coll,(s => {
											if(!(cloudfiles inTagFolder) || !(cloudfiles isTagFolder) ) {
												cloudfiles.moveCollection(current_coll,s);
												current_coll = s
											}
											})
										),
										() => {Noop}
									),
									"delete" -> <div>
										<lift:WebDrive.deleteCollection form ="POST">
										<input type="hidden" name="delpath" value={coll}/>
										<del:submit/>
										</lift:WebDrive.deleteCollection>
										</div>
										)
							}
						}		
					}
				}
			})
		}
		
		def deleteCollection(xhtml: NodeSeq): NodeSeq = {
			//handle the submit based on the delpath parameter
			//Redirect to the same page (or do a SetHtml)
			bind("del",xhtml,"submit" ->
				SHtml.submit(S ? "Delete",() => {
					val delpath = stripLeadingSlash(S.param("delpath").openOr("/dev/null"))
					val respath = stripTrailingSlash(resource)
					debug("&&&***&&& Delete resource (delpath): "+ delpath)
					debug("&&&***&&& in resource path: "+respath)
					cloudfiles.deleteCollection(respath + "/" + delpath)
				})
			)			
		}

		def delete(xhtml: NodeSeq): NodeSeq = {
			//handle the submit based on the delpath parameter
			//Redirect to the same page (or do a SetHtml)
			bind("del",xhtml,"submit" ->
				SHtml.submit(S ? "Delete",() => {
					val delpath = stripLeadingSlash(S.param("delpath").openOr("/dev/null"))
					val respath = stripTrailingSlash(resource)
					debug("&&&***&&& Delete resource (delpath): "+ delpath)
					debug("&&&***&&& in resource path: "+respath)
					cloudfiles.deleteFile(respath + "/" + delpath)
				})
			)

		}
		
		def label_gen(resource: String) = {
			SHtml.text(cloudfiles.getTags(resource).mkString(" "),(tags: String) => {
					cloudfiles.setTags(resource,tags.split(" ").map(_.trim).filterNot(_.isEmpty).toList)
				}
			)
		}
		
		private def normalizeFilename(filename: String) = {
			val index = filename.indexOf("points to source file") match {
				case -1 => filename.length
				case n: Int => n
			}
			filename.slice(0,index)
		}
		
		def listResources(xhtml: NodeSeq): NodeSeq = {
			
			cloudfiles inTagFolder match {	
				
				//CODE_CC div arroud SHtml.link to allow SetHtml upon rename?		
				case true => {	
					resources.flatMap(res => {bind("res",xhtml,
					"item" -> SHtml.link("/download"+stripTrailingSlash(resource)+"/"+res,nullfunc,Text(normalizeFilename(res))),
						"edit" -> SHtml.ajaxEditable(Text("Metadata"),label_gen(res),() => Noop),
						"delete" -> <div></div>)
					})
				}
				
				case false => {						
					
					resources.flatMap(res => {
						
						var current_res = res
						
						bind("res",xhtml,
						"item" ->SHtml.ajaxEditable(
							SHtml.link("/download"+stripTrailingSlash(resource)+"/"+current_res,nullfunc,Text(current_res)),
							SHtml.text(current_res,(s => {cloudfiles.moveFile(current_res,s);current_res = s})),
							() => {Noop}
						),
						"edit" -> SHtml.ajaxEditable(Text("Metadata"),label_gen(res),() => Noop),
						"delete" -> <div>
							<lift:WebDrive.delete form ="POST">
							<input type="hidden" name="delpath" value={res}/>
							<del:submit/>
							</lift:WebDrive.delete>
						</div>)
					})
				}				
				
			}
		}
				
		private def generateCollections(): NodeSeq = {
			val collections = (cloudfiles.listSubCollections).sortWith((e1, e2) => (e1 compareTo e2) < 0)
			val xhtml = <div><coll:item/></div>
			val result = collections.flatMap(coll => bind("coll",xhtml,
				"item" -> SHtml.link("/webdrive"+stripTrailingSlash(resource)+"/"+coll,nullfunc,Text(coll)))
			)
			result
		}
		
		def allowNewCollection(xhtml: NodeSeq):NodeSeq = {
			
			if(!cloudfiles.inTagFolder) {
				bind("newcollection",xhtml,"allowed"  ->
					<div><lift:WebDrive.newCollection><new:coll/></lift:WebDrive.newCollection></div>
				)
			} else <div></div>
		}
		
		def newCollection(xhtml: NodeSeq): NodeSeq = {
			
			bind("new",xhtml,"coll" -> SHtml.ajaxEditable(Text(S ? "Click Edit to add a folder"),SHtml.text("",
			(s: String) => {
					cloudfiles.createCollection(cloudfiles.fullkey +"/" + s)
				}),
				() => SetHtml("collections",generateCollections())))				
		}
		
		def allowUploader(xhtml: NodeSeq): NodeSeq = {
			if(!cloudfiles.inTagFolder && !cloudfiles.isTagFolder) {
				bind("upload",xhtml,"allowed" -> 
				<div>
					<lift:WebDrive.uploader form ="POST" multipart="true">
					<upload:loader/>
					<upload:submit/>
					</lift:WebDrive.uploader>
				</div>								
				)
			}
			else <div></div>
		}
		
		def tagText(xhtml: NodeSeq):NodeSeq = {
			if(!cloudfiles.inTagFolder) {
				
				val text = S ? "You can attach metadata to files below. These can be used to filter and search quickly. These searches can be stored as special 'Metadata folders' and can be found as smart subfolders of the 'Metadata search' folder you see above."
				
				bind("some",xhtml,"text" -> 
				<div>{text}</div>
				)
			} else <div></div>
		}
		
		def uploader(xhtml: NodeSeq): NodeSeq = {
			bind("upload",xhtml,
			"loader" -> SHtml.fileUpload((f) => {fileHolder = new Full(f)}),
			"submit" -> SHtml.submit(S ? "Upload",() => {cloudfiles.finalizeUpload(fileHolder)})
				)
		}
	
		def searchBox(xhtml: NodeSeq): NodeSeq = {
			bind("search",xhtml,"box" -> SHtml.text("",(tags) => {S.redirectTo("/search",() => { 
					current_tags(tags.split(" ").map(_.trim).filterNot(z => z.isEmpty).toList)
				}
				)
			}),"submit" -%> SHtml.submit(S ? "Search",() => {}))
		}
		
		def searchresults(xhtml: NodeSeq): NodeSeq = {
			val userlength = 1 + cloudfiles.user.length
			//Get the tagged resources
			val tagged_resources = cloudfiles.hasTags(current_tags.is).map(x => x.slice(userlength,x.length))
			current_tagged_resources(tagged_resources)
			//build the download links
			tagged_resources.flatMap(res => bind("search",xhtml,
			"item" ->SHtml.link("/download"+res,nullfunc,Text(res.slice(1,res.length)))			
			)
			)
			
		}
		
	
		def saveSearch(xhtml: NodeSeq): NodeSeq = {
			
			var foldername = ""
			val tags = current_tags.is
			val links = cloudfiles.hasTags(tags)
			
			debug("in saveSearch, links for VM are: "+links)
			
			bind("search",xhtml,
				"tagnames" -> Text(S ? "Save search for tags "+tags+ " as: "),
				"foldername" -> SHtml.text(tags.mkString(" "),(x) => {foldername = x}), 
				"submit" -> SHtml.submit(S? "Save folder",() => {
					cloudfiles.saveTagFolder(foldername,links,tags)
					S.redirectTo("/webdrive" + "/" + stripLeadingSlash(Config("tag_folder_root"))  + "/" + foldername)
				})
			)
			
		}
	
		
	}	
}