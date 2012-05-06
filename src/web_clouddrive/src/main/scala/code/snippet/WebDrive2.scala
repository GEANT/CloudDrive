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

	object current_tags2 extends RequestVar[List[String]](List())
	object current_tagged_resources2 extends RequestVar[List[String]](List())

  	class WebDrive2 extends Treatise {
	
		
    	val headers = Map("resource" -> S.param("resource").openOr("/"));

		val resource = headers("resource")
		val cloudfiles = new CloudFiles(headers)
		val nullfunc = {() => ()}
		lazy val collections = (cloudfiles.listSubCollections).sortWith((e1, e2) => (e1 compareTo e2) < 0)
	    lazy val resources = (cloudfiles.listSubResources).sortWith((e1, e2) => (e1 compareTo e2) < 0)
	    var fileHolder : Box[FileParamHolder] = Empty
		
		//Done
		def dirHeader(xhtml:NodeSeq): NodeSeq = {
			if (collections.length > 0) {
				cloudfiles.inTagFolder() match {
					case true => bind("dir",xhtml,"title" -> <b>{S ? "Smart folders"}</b>)
					case false => bind("dir",xhtml,"title" -> <b>{S ? "Dirs"}</b>)
				}
			}
			else bind("dir",xhtml,"title" -> "")
		}
		
		//Done
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
				resources.flatMap(item => {
					val item_name = item match {
						case s: String if (s == "/") => { S ? "Home"}
						case _ => item
					}
					bind("bread",xhtml,"crumbs" -> SHtml.link(stripTrailingSlash("/webdrive" + item),nullfunc,Text(stripLeadingSlash(item_name))),"separator" -> Text("  >>  "))})
			} else bind("bread",xhtml,"crumbs" -> "","separator" -> "")
		}
		
		def listCollections(xhtml: NodeSeq): NodeSeq = {
			collections.flatMap(coll => {
				
				val coll_path = stripTrailingSlash(resource) + "/" + stripTrailingSlash(coll)
				var current_coll = coll
				val delete_image = <img src="/classpath/images/delete_file.png"/>
				val folder_image = <img src="/classpath/images/folder.png"/>
				
				debug("$$$ isTagFolder in listCollections for "+coll_path+" = "+cloudfiles.isTagFolder_?(coll_path))
				
				cloudfiles.isMandatoryFolder(coll_path) match {
					
					case true => {
						bind("coll",xhtml,
							"item" -> <div>
								{folder_image}
								{SHtml.link("/webdrive"+stripTrailingSlash(resource)+"/"+coll,nullfunc,Text(coll))} 
							</div>
							)
					}
							
					
				
					case false 	=> {
						
						cloudfiles.isTagFolder_?(coll_path) match {
							
							case true => {
								bind("coll",xhtml,
									"item" -> 
									<div>
										{folder_image}
										{SHtml.link("/webdrive"+stripTrailingSlash(resource)+"/"+coll,nullfunc,Text(coll))}
										{SHtml.link("/webdrive",{() => cloudfiles.deleteCollection(coll_path)},delete_image)}
									</div>)
							}
						
							case false => {
								bind("coll",xhtml,
									"item" -> SHtml.ajaxEditable(
										<span>
											{folder_image}
											{SHtml.link("/webdrive"+stripTrailingSlash(resource)+"/"+current_coll,nullfunc,Text(current_coll))}
											{SHtml.link("/webdrive",{() => cloudfiles.deleteCollection(coll_path)},delete_image)}
										</span>,
											
										SHtml.text(current_coll,(s => {
											if(!(cloudfiles inTagFolder) || !(cloudfiles isTagFolder) ) {
												cloudfiles.moveCollection(current_coll,s);
												current_coll = s
											}
										})),
										() => {Noop})
									 )
							}
						}	
					}
				}
			})
		}
		

		def label_gen(resource: String) = {
			SHtml.ajaxText(cloudfiles.getTags(resource).mkString(" "),
					(tags: String) => {cloudfiles.setTags(resource,tags.split(" ").map(_.trim).filterNot(_.isEmpty).toList)}
			)
		}
		
		//Done
		private def normalizeFilename(filename: String) = {
			val index = filename.indexOf("points to source file") match {
				case -1 => filename.length
				case n: Int => n
			}
			filename.slice(0,index)
		}
		
		//Done
		def listResources(xhtml: NodeSeq): NodeSeq = {
			
			cloudfiles inTagFolder match {	
				
				case true => {	
					
					resources.flatMap(res => {
						
						var current_res = res
						val download_image = <img src="/classpath/images/download_arrow.png"/>
						
						bind("res",xhtml,
						"download" -> SHtml.link("/download"+stripTrailingSlash(resource)+"/"+current_res,nullfunc,download_image),
						"delete" -> <span></span>,
						"item" -> Text(current_res),
						"edit" -> label_gen(res)
					)})
				}
				
				case false => {						
					
					resources.flatMap(res => {
						
						var current_res = res
						val download_image = <img src="/classpath/images/download_arrow.png"/>
						val delete_image = <img src="/classpath/images/delete_file.png"/>
						bind(
							"res",xhtml,
							"download" -> SHtml.link("/download"+stripTrailingSlash(resource)+"/"+current_res,nullfunc,download_image),
							"delete" -> SHtml.link("/webdrive",() => cloudfiles.deleteFile(stripTrailingSlash(resource)+"/"+res),delete_image),
							"item" ->SHtml.ajaxText(current_res, ( s => {cloudfiles.moveFile(current_res,s);current_res = s}),"class" -> "input-filename"),
							"edit" -> label_gen(res)
						)
					})
				}				
			}
		}

		
		//Done
		def allowNewCollection(xhtml: NodeSeq):NodeSeq = {
			
			if(!cloudfiles.inTagFolder) {
				bind("newcollection",xhtml,"allowed"  ->
					<div><lift:WebDrive2.newCollection><new:coll/></lift:WebDrive2.newCollection></div>
				)
			} else <div></div>
		}
		
		def newCollection(xhtml: NodeSeq): NodeSeq = {
			
			bind("new",xhtml,"coll" -%> SHtml.swappable(<img src="/classpath/images/add_swappable.png"></img>,
			SHtml.ajaxText(S ? "New Folder",(s: String) => {
					cloudfiles.createCollection(cloudfiles.fullkey +"/" + s)
				//}) //,
				RedirectTo(cloudfiles.resource)				
				})))
		}
		
		//Done
		def allowUploader(xhtml: NodeSeq): NodeSeq = {
			if(!cloudfiles.inTagFolder && !cloudfiles.isTagFolder) {
				bind("upload",xhtml,"allowed" -> 
				//this should only add a container for the swappable and call Webdrive2.uploader
				<lift:WebDrive2.uploader>
				<upload:uploader/>
				</lift:WebDrive2.uploader>								
				)
			}
			else <div></div>
		}
		

		//Done
		def handleUpload(xhtml: NodeSeq):NodeSeq = {
			bind("upload",xhtml,
			"loader" -> SHtml.fileUpload((f) => {fileHolder = new Full(f)}),
			"submit" -> SHtml.submit(S ? "Upload",() => {cloudfiles.finalizeUpload(fileHolder)})
				)			
		}
		
		//Done
		def uploader(xhtml: NodeSeq): NodeSeq = {
			//he bind needs to generate a root element for a swappable
			//With (a) the + image, and as hidden element (b) the upload form div
			//bind("upload",xhtml,"uploader" -> SHhtml.swappable(<img tag>,<div> ...Webdrive2 upload form </div>))
			//Note that we need a handleUpload method that mirrors the original uploader method then
			val upload_snippet = <div><lift:WebDrive2.handleUpload form ="POST" multipart="true" class="upload-form"><upload:loader/><upload:submit/></lift:WebDrive2.handleUpload></div>
			bind("upload",xhtml,"uploader" -%> SHtml.swappable(<img src="/classpath/images/add_swappable.png"></img>,upload_snippet))
		}
		
		
		//Done
		def searchBox(xhtml: NodeSeq): NodeSeq = {
			bind("search",xhtml,"box" -> SHtml.text("",(tags) => {S.redirectTo("/search",() => { 
					current_tags2(tags.split(" ").map(_.trim).filterNot(z => z.isEmpty).toList)
				}
				)
			}),"submit" -%> SHtml.submit(S ? "Search",() => {}))
		}

		//Done
		def tagText(xhtml: NodeSeq):NodeSeq = {
			if((!cloudfiles.inTagFolder) && (resources.length > 0)) {
				
				val text = S ? "You can attach tags to files below. These can be used to filter and search quickly. These searches can be stored as special 'Search folders' and can be found as smart subfolders of the 'Stored searches' folder you see above."
				
				bind("some",xhtml,"text" -> 
				<div>{text}</div>
				)
			} else <div></div>
		}

		//Done
		def searchresults(xhtml: NodeSeq): NodeSeq = {
			val userlength = 1 + cloudfiles.user.length
			//Get the tagged resources
			val tagged_resources = cloudfiles.hasTags(current_tags2.is).map(x => x.slice(userlength,x.length))
			current_tagged_resources2(tagged_resources)
			//build the download links
			tagged_resources.flatMap(res => bind("search",xhtml,
			"item" ->SHtml.link("/download"+res,nullfunc,Text(res.slice(1,res.length)))			
			)
			)
			
		}
		
		//Done
		def saveSearch(xhtml: NodeSeq): NodeSeq = {
			
			var foldername = ""
			val tags = current_tags2.is
			val links = cloudfiles.hasTags(tags)
			
			debug("in saveSearch, links for VM are: "+links)
			
			bind("search",xhtml,
				"tagnames" -> Text(S ? "Save search for tags "+tags.mkString(" ")+ " as: "),
				"foldername" -> SHtml.text(tags.mkString(" "),(x) => {foldername = x}), 
				"submit" -> SHtml.submit(S? "Save folder",() => {
					cloudfiles.saveTagFolder(foldername,links,tags)
					S.redirectTo("/webdrive" + "/" + stripLeadingSlash(Config("tag_folder_root"))  + "/" + foldername)
				})
			)
			
		}
	
		
	}	
}