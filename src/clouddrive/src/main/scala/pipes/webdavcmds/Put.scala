import net.vrijheid.clouddrive.pipes._
import net.vrijheid.clouddrive.sharing._
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.providers.{Locker}
import net.vrijheid.clouddrive.httpsupport.{HTTPServerHelper}
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes.webdavcmds {
	
	//
	class PUTSink[T](implicit val ctx: RootContext[T])  extends Sharing[T] with PipeSink  with Serializable {
		
		private var content_length = 0
		private var fullkey: String = ""
		private var allowed = false
		private var tagdestination = false
		private var common_key: String = ""
		private var tags: List[String] = List()
		
		//Helper function to make sure we have unique filenames in our "Comon" bin
		private def uniquely(path: String) = {
			debug("Generating a unique file name in common_folder_root")
			var newpath = path
			var counter = 0
			while(exists(newpath)) {
				newpath = path+"("+counter+")"
				counter +=1
			}
			debug("Unique name = "+newpath)
			newpath
		}
		
		
		//First, lets' try and open or store in write mode
		override def ||() { 
			
			debug("In PUTSink, initializing FileStore")
			fullkey = (stripTrailingSlash("/" + ctx.user + ctx.verb.header("resource")))
			val fullpath = pathFromFullPath(fullkey)
			
			if(!allowedAccess(fullkey,ctx.user,ctx.verb)) {
				debug("PUT: opening store, No Access. Generating 403 response.")
				val header = (HTTPServerHelper httpHeader(403,"text/plain","")) getBytes;
				ctx.bypass.write(header)
				ctx.bypass.close()	
				//Necessary to kill the thread
				throw new Exception()
			}
			
			//TODO_TAG
			//Again this elaborate condition prohibits PUT in Tag folders.
			//Late, we'll just add a link, create the file inc "/Common" and set its tags
			allowed = (allowed(followLink(fullkey)))
			//If it's a tagfolder, set the flag, create a key for the Coomon folder, and
			//create a new store; also prefetch the tags
			(isTagFolder_?(fullpath)) match {
				case true => {
						//At this point we already have a bluntly created a key, so we try and delete it
						hopefully{
							debug("Is link fullkey, inside hopefully:"+isLink(fullkey))
							if(exists(fullkey) && !isLink(fullkey)) {delete(fullkey)}
						}
						debug("***we have something in  tagfolder")
						val filename = (fileNameFromPath(ctx.verb.header("resource")))
						common_key = uniquely("/"+ctx.user+"/"+Config("common_folder_root")+"/"+filename)
						//In this case, we MUST change ctx.recomputeFullPath to return the unique metadata name
						ctx.recomputeFullPath = {(s: String) => {debug("---recomputed path = "+fullkey);fullkey}}
						//This gullkey is actually the name of the symlink
						fullkey = uniquely(fullkey)
						debug("!!!--!!! common,full key "+common_key+","+fullkey)
						debug("***common key = "+common_key)
						ctx.store = Storage.mkStorage(ctx.userConfig("storage"),common_key)
						tagdestination = true
						tags = getTagFolderTags(fullpath)
						debug("***tags are "+tags)
					}
				case false =>  {
					//Only do this when we are NOT in a serach folder; otherwise we can only uodate and not add files with the same name to a tag folder
					fullkey = followLink(fullkey)
				}
			}
			if (allowed) { ctx.store.open('write ) }
		}
 		
		//Write whatever dat we have to the file
		override def >|(data : Array[Byte])  {
			debug("In >|, appending data")
			if(allowed && !(null == data)) {
				ctx.store write(data)
				content_length += (data length)
			}
		}
		
		override def |>|(data: Array[Byte]) {
			debug ("In |>|, commiting data")
			//If it is allowe and there is any remaining data, write it
			if(allowed &&(!(data isEmpty))) {
				ctx.store write(data)
				content_length += (data length)
			}
			//This is called here, so that we can control buffering/etc on the storage layer.
			ctx.store.transfer;
		}
	
		override def <| ()  : Array[Byte] = {
			
			debug ("in <|")
			ctx.phase = 'allwritten
			
			//Close the store
			if(allowed){

				//Create the zk node now that all data has been written, unless it already exists,
				//then we rest the original			
			
				debug("Creating init vars")
				val resource = ctx.verb.header("resource")
				val displayname = (stripTrailingSlash(resource) split "/") last
			
				//Check for exists fullkey and make an if/else
				tagdestination match {
					case false =>
					{
						debug("No tg destination for PUT, adding directly")
						if(exists(fullkey)) {
							debug("Update, setting metadata")
							//Update necessary fields
							try {
								setMetaData(fullkey,davEncode("getcontentlength"),ctx.actualContentLength toString)
								setMetaData(fullkey,davEncode("getlastmodified"),idateNow)
								setMetaData(fullkey,davEncode("getcontenttype"),"application/binary")
								setMetaData(fullkey,davEncode("getetag"),UUID())
								setMetaData(fullkey,davEncode("resourcetype"),"")
			          			setPutLength(fullkey,ctx.putContentLength)
							}
							catch { case e => {
								//e printStackTrace}
								}
							}
						} else {
							debug("Created in metadata back end")
							//Create it all
		          			createResource(fullkey)				
							//Set the metadata
							setMetaData(fullkey,davEncode("getcontentlanguage"),"en")
							setMetaData(fullkey,davEncode("getcontentlength"),ctx.actualContentLength toString)
							setMetaData(fullkey,davEncode("getcontenttype"),"application/binary")
							setMetaData(fullkey,davEncode("getetag"),UUID())								
							//TBD only creationdate on new resource
							setMetaData(fullkey,davEncode("creationdate"),idateNow)
							setMetaData(fullkey,davEncode("displayname"),displayname)
							setMetaData(fullkey,davEncode("getlastmodified"),idateNow)
							setMetaData(fullkey,davEncode("resourcetype"),"")
							setMetaData(fullkey,davEncode("source"),"")
							setPutLength(fullkey,ctx.putContentLength)
						}
					}
						
					case true => {
						debug("tag_destination for put")
	          			createResource(common_key)	
						debug("createResource executed for common_key "+common_key)
						//Set the metadata
						setMetaData(common_key,davEncode("getcontentlanguage"),"en")
						setMetaData(common_key,davEncode("getcontentlength"),ctx.actualContentLength toString)
						setMetaData(common_key,davEncode("getcontenttype"),"application/binary")
						setMetaData(common_key,davEncode("getetag"),UUID())								
						//TBD only creationdate on new resource
						setMetaData(common_key,davEncode("creationdate"),idateNow)
						setMetaData(common_key,davEncode("displayname"),displayname)
						setMetaData(common_key,davEncode("getlastmodified"),idateNow)
						setMetaData(common_key,davEncode("resourcetype"),"")
						setMetaData(common_key,davEncode("source"),"")
						setPutLength(common_key,ctx.putContentLength)
						debug("All metadata set on common_key")
						setTags(common_key,tags)
						debug("Tags "+tags+" set "+" om common_key")
						createLink(this.common_key,this.fullkey,Map())	
						debug("...and link created from "+common_key+" to "+fullkey)						
					}

				}

				debug("Returning 201 header")
				(HTTPServerHelper.httpHeader(201,"text/plain",0)) getBytes;
			}
			else {
				debug("Returning 423 header")
				(HTTPServerHelper.httpHeader(423,"text/plain",0)) getBytes;				
			}
		}
	}
}