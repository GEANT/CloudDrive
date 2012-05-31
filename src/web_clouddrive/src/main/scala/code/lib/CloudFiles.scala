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
			
			import net.vrijheid.clouddrive.website.code.lib.login._
			import net.vrijheid.clouddrive.control._
			import net.vrijheid.clouddrive.pipes._
			import net.vrijheid.clouddrive.providers._
			import net.vrijheid.clouddrive.config._
			import net.vrijheid.clouddrive.utils._ 
			import net.vrijheid.clouddrive.httpsupport._
			import net.liftweb.common.{ Box, Full,Empty }
			import net.liftweb.util.Helpers._
		  	import net.liftweb.http.{S, SHtml,FileParamHolder,OnDiskFileParamHolder }
			import java.io.{File,FileInputStream,FileOutputStream}
			import com.amazonaws._
			import voldemort.versioning._
			
			
			//TBD: CHeck to refactor this acrross clouddrive, it's a repeat right now
			//Not sure if that's easy feasib;e - backburner
			trait LiftMetering extends Utils  {
				
				val dav_namespace_uri = "DAV"
				val dav_namespace_abbrev = "D"
				val dav_namespace = "xmlns:D=\"DAV:\""

				def davEncode(key : String) : String = {
					encodeNamespace(dav_namespace,key)
				}

				//TBD: ugly until we have logback/log4j
				def debug(msg : String) {
					if(Config("loglevel") == "debug") {scala.Console println(msg)}
				}

				def isDavEncoded(nskey : String) = {
					(extractNamespaceURI(nskey) == (extractNamespaceURI(dav_namespace)))
				}

				def davDecode(nskey : String) = {
					val (ns,key) = decodeNamespace(nskey)
					if (isDavEncoded(ns)) { extractNamespace(ns) + ":" + key}
					else {throw new Exception("Not a DAV namespace")}

				}
				
				//This is he function to correct when having stale data: simply read again and add orur data
				val add_stale_update = {(current: Long, delta: Long) => {current + delta}}
				//This is a first class function to change the GBMonth couters
				@serializable
				val add_gbmonth_update = {
					(current: GBMonth,delta: (Long,Long)) => {
						debug("in add_gbmonth_update delta function")
						//delta is a tuple of (size,usage)
						current.total_size += delta._1;
						current.usage += delta._2;
						debug("new value is: " + current.toString)
						current
					}
				}

				def sizeOfPath(path: String,ctx : LiftContext,metadata: MetaData[String]): Long = {
					//We get the tree, map each path to its length, and sum using foldLeft
					((metadata.treeAsList(path)).map { x => metadata.getPutLength(x)}).foldLeft(0L){ (acc: Long,x: Long) => {acc + x}}
				}


				def initGBMonth(user: String)(implicit gbm: VMClient[String,GBMonth]): Versioned[GBMonth] = {

					debug("In initGBMonth")
					//The idea is that we get the previous month, read it and set the current months totals that way
					//if there is no previous month, then this month is the first, and we initialize it with 0Ls
					val current_month = userMonthYear(user)
					val previous_month = previousUserMonthYear(user)
					val previous_gb_month = gbm get previous_month;
					if (previous_gb_month == null) {
						//First month
						debug("new month, first time")
						gbm init(current_month,new GBMonth(0,0))
					} else {
						//Initialize with the values of last month
						debug("new month, fetching value of last month")
						val gbmonth = previous_gb_month getValue;
						debug("updating usage for new month")
						gbmonth.usage = (gbmonth.total_size * hoursOfThisMonth)
						debug("... and init on the new month with the updated values")
						gbm init(current_month,gbmonth)
					}
				}

				def initNextGBMonth(user: String)(implicit gbm: VMClient[String,GBMonth]): Versioned[GBMonth] = {

					//debug("In initNextGBMonth")
					//The idea is that we get the previous month, read it and set the current months totals that way
					//if there is no previous month, then this month is the first, and we initialize it with 0Ls
					val current_month = userMonthYear(user)
					val next_month = nextUserMonthYear(user)
					val next_gb_month = gbm get current_month;
					//debug("current,next month keys = " + current_month + " , " + next_month)

					if (next_gb_month == null) {
						//First month
						//debug("new month without predecessor, initialzing")
						gbm init(next_month,new GBMonth(0,0))
					} else {
						//Initialize with the values of last month
						//debug("new month, with predecessor")
						val gbmonth = next_gb_month getValue;
						gbmonth.usage = (gbmonth.total_size * hoursOfNextMonth)
						//debug("gbmonth for next month will be: " + gbmonth.toString)
						gbm init(current_month,gbmonth)
					}
				}

				def getGBMonth(user: String)(implicit gbm: VMClient[String,GBMonth]): Versioned[GBMonth] = {

					//debug("In getGBMonth")
					//We get the current month, if there is n1L there, we create 1L based on the previous month (initGBMonth)
					val current_month = userMonthYear(user)
					val current_gb_month = gbm get current_month
					if(null == current_gb_month) {
						//debug("no current_gb_month yet, call initGBMonth")
						initGBMonth(user)
					} else {
						//debug("Return current, existing gbmonth")
						current_gb_month;
					}
				} 
				
				def ensureMetering(user: String)(implicit gbm: VMClient[String,GBMonth]) = {
					
					val hours = (hoursOfThisMonth - currentMonthHour)
					//debug("hours left this month: " + hours.toString)
					//We only do this so that the actual key/value for this month is initialized
					val month: Versioned[GBMonth] = hours match {

						case 0  => {
							debug("No more hours left this month, get hours of next month")
							//We are at the end of the month
							//debug("...and initalizing next month")
							initNextGBMonth(user)
						}

						case _ => {
							//debug("hours left, get the current GBMonth")
							getGBMonth(user)
						}
					}
					
				}
				
			}
			
			
			/**
			This class is the entry point for all information, downloads pipes etc for a specific 
			resource. If no "resource" ket is present in the headers Map, "/" is assumed.
			The user is set up based on the Login session user.
			
			All other classes are inferred magically.
			*/
			class CloudFiles(headers: Map[String,String]) extends LiftMetering {
						
				//Init the user
				val user = Login.getUserEmail.openOr("")
				val resource = ctx.verb.header("resource")
				//Create metering infra
				implicit val gbmonth_meterer = VMTalk getGBMonthMeterer;

				//Create a "fake" HTTP verb to work with drive context
				lazy val verb = (headers contains "resource") match {
					case true => new WEBSITE(headers,'prop)
					//Safeguard
					case false => new WEBSITE(headers ++ Map("resource" -> "/"),'prop)
				}
				//Create the drive context and set some initials
				implicit lazy val ctx = new LiftContext(verb)
				ctx.user = user
				ctx.userConfig = Config.userConfig(ctx)
				ctx.storageclient = VMTalk.getStorageClient;
				ctx.store = Storage.mkStorage(
					ctx.userConfig("storage"),
					(ctx.verb.header("resource"))
				)
				//Ensure entries for this month
				ensureMetering(ctx.user)
				//Create a metatdata store, if needed
				lazy val metadata = new MetaData()
				//The full metadata key to our file
				lazy val fullkey = stripTrailingSlash("/" + user + verb.header("resource"))

				def listCollection() = {
					metadata.getChildren(fullkey)
				}
				
				def listSubCollections() = {
					listCollection.filter( x => {
						metadata.isCollection(fullkey + "/" + x)
					}
					)
				}
				
				def getTags(resource: String) = {
					metadata getTags(fullkey +"/" + resource)
				}
				
				def setTags(resource: String,tags: List[String]) {
					val fullpath = fullkey+"/"+resource
					val fullsource = metadata followLink(fullpath)
					//Update all search results

					//Delete all the currently linked links, including ourselves
					//This ensures that we still satisfy (as VMLink) the tag folder's search conditions
					//We will revive where appropiate usinf sync2Tags below
					
					metadata deleteFileFromTagSets(ctx.user,fullsource)

					//Set the new metadata on the source
					metadata setTags(fullsource,tags)
					//Update search results. Fullsource will be fullpath if it's no link, so we always end up right
					metadata sync2Tags(user,fullsource) 

				}				
				
				def listSubResources() = {
					listCollection.filterNot( x => metadata.isCollection(fullkey + "/" + x))
				}
				
				def moveFile(source: String,destination: String) {
					
					//Set up the full source, destination paths
					val full_resource = fullkey +"/" + source
					val full_destination = fullkey + "/" + destination
					debug("full_resource in moveFile = "+full_resource)
					debug("full_destination in moveFile = "+full_destination)
					
					//Create the new node
					if(! metadata.exists(full_destination)) {
						metadata createResource(full_destination)
					}
					debug("destination created, now transfering metadata")
					//Transfer the metadata
					metadata copyResourceData(full_resource,full_destination)
					debug("metadata transfered")
					//Remove old source from the tagsets
					metadata deleteFileFromTagSets(user,full_resource)
					debug("Deleting old file from tagsets")
					//Add the destination to the tagsets
					metadata addFileToTagSets(user,full_destination)
					debug("Adding new file from tagsets")
					//Delete the old one
					metadata delete(full_resource)
					debug("original resource deleted")
				}
				
				def moveCollection(source: String,destination: String) {
					
					debug("---Entering moveCollection")
					//Set up the full source, destination paths
					val full_resource = fullkey +"/" + source
					val full_destination = fullkey + "/" + destination					
					
					debug("full_destination = " + full_destination)
					if(! (metadata exists(full_destination))) {
						metadata createCollection(full_destination)
						metadata copyResourceData(full_resource,full_destination)
					}
					
					debug("MOVE, isCollection")
					//Get the prefix from the source (its index)
					//val prefix = findIndexOfReverse(full_resource,slash)
					val prefix = (full_resource length) + 1
					//Get the resource list, and reverse so we go from top to bottom
					//Drop the first element,as that is the actual root folder we're moving
					val resources = ((metadata treeAsList(full_resource)) reverse).drop(1)
					debug("resources as list = " + resources)
					//Loop over the list
					resources foreach {
						(resource) => {
							//Destination = destination_path + (resource - prefix)
							val destination = stripTrailingSlash(full_destination + "/" + (safeSubstring(resource,prefix)))
							debug("Deep copy, destination is: " + destination)
							debug("Deep copy, resource is: " + resource)
							//create destination, copy resource data, delete
							//If it exists, delete
							if (metadata exists(destination)) { 
								debug("destination " + destination + " exists")
								metadata deleteTree(destination)
							}
							//Create the metadata representation
							metadata isCollection(resource) match {
								case true => {
									createCollection(destination)
									metadata copyResourceData(resource,destination)
								}
								
								case false => {
									//Create the resource
									metadata createResource(destination)
									metadata copyResourceData(resource,destination)
									metadata delete(resource)
									metadata deleteFileFromTagSets(ctx.user,resource)											
								}
							}
							debug("***children of destination = "+metadata.getChildren(destination))
							//Transfer the metadata
							if(!(metadata.isCollection(destination))) {
								metadata.addFileToTagSets(ctx.user,destination)
								debug("Added new resource in collection move to tagset")
							}
							debug("***After resource copy, children of destination = "+metadata.getChildren(destination))
							

						}
					}
					//Finally, if all succeeds, delete the source
					metadata deleteTree(full_resource)	
					debug("---exiting moveCollection")				
				}
				
				def deleteFile(path: String) {
					debug("In deleteFile, incoming path = "+path)
					val fullpath = "/" + ctx.user + path
					val moi_now = userMonthYear(ctx.user)
					debug("In CloudFiles.deleteFile, fullpath = "+fullpath)
					//create storage for this path
					val store = Storage.mkStorage(ctx.userConfig("storage"),fullpath)
					//delete the file
					store.delete()
					// lower GBMONTH
					var hours = (hoursOfThisMonth - currentMonthHour)
					debug("****hours left this month: " + hours.toString)
					var key = userMonthYear(ctx.user);
					debug("Key = " + key)

					//We only do this so that the actual key/value for this month is initialized
					val month: Versioned[GBMonth] = hours match {

						case 0  => {
							debug("No more hours left this month, get hours of next month")
							//We are at the end of the month, so or #hours is the hours of next month
							//Key needs to change, tpoo
							hours = hoursOfNextMonth
							debug("Updating key to next month")
							key = nextUserMonthYear(ctx.user);
							debug("...and initalizing next month")
							initNextGBMonth(ctx.user)
						}

						case _ => {
							debug("hours left, get the current GBMonth")
							getGBMonth(ctx.user)
						}
					}
					val storage_length = sizeOfPath(fullpath,ctx,metadata)
					
					val delta : (Long,Long) = (-1 * storage_length, -1 * (storage_length * hours))
					debug("DELETE, delta = " + delta)
					gbmonth_meterer applyDelta(key,delta,add_gbmonth_update)					
					
					metadata deleteTree(fullpath)
				}
				
				def deleteCollection(path: String) {
					debug("In deleteCollection, incoming path = "+path)
					val moi_now = userMonthYear(ctx.user)
					val fullpath = "/" + ctx.user + path
					debug("In CloudFiles.deleteCollection, fullpath = "+fullpath)
					//create storage for this path
					metadata inTagFolder(fullpath) match {
						
						case true => {
							val foldername = fileNameFromPath(fullpath)
							debug("Deleting tag folder")
							val deleter = { 
								(key : String) => {
									debug("In deleter, key = " + key)
									val new_key = (key substring (1 + (ctx.user length)))
									metadata deleteTree(key)
								}
							}
							metadata processTree(fullpath,deleter)
							metadata removeFromTagSet(ctx.user,foldername)
						}
						
						case false => {
							debug("deleting actual folder and contents") 
							val moi_now = userMonthYear(ctx.user)
							val storage_length = sizeOfPath(fullpath,ctx,metadata)
							
							val deleter = { 
								(key : String) => {
									debug("In deleter, key = " + key)
									debug("making storage")
									val new_key = (key substring (1 + (ctx.user length)))
									val store = Storage.mkStorage(ctx.userConfig("storage"),new_key)
									debug("store made")
									store.delete();
									metadata deleteTree(key)
								}
							}
							metadata processTree(fullpath,deleter)
							//NOW lower the totalsize and GBmonth estimates
							var hours = (hoursOfThisMonth - currentMonthHour)
							debug("****hours left this month: " + hours.toString)
							var key = userMonthYear(ctx.user);
							debug("Key = " + key)

							//We only do this so that the actual key/value for this month is initialized
							val month: Versioned[GBMonth] = hours match {

								case 0  => {
									debug("No more hours left this month, get hours of next month")
									//We are at the end of the month, so or #hours is the hours of next month
									//Key needs to change, tpoo
									hours = hoursOfNextMonth
									debug("Updating key to next month")
									key = nextUserMonthYear(ctx.user);
									debug("...and initalizing next month")
									initNextGBMonth(ctx.user)
								}

								case _ => {
									debug("hours left, get the current GBMonth")
									getGBMonth(ctx.user)
								}
							}

							val delta : (Long,Long) = (-1 * storage_length, -1 * (storage_length * hours))
							debug("DELETE, delta = " + delta)
							gbmonth_meterer applyDelta(key,delta,add_gbmonth_update)
						}
					}
				}
				
				def createCollection(fullkey: String) {
					
					metadata createCollection(fullkey)
					val now = isoDateNow()
					metadata setMetaData(fullkey,Map(davEncode("resourcetype") -> ("<" + dav_namespace_abbrev + ":collection/>"),davEncode("creationdate") -> now, davEncode("getlastmodified") -> now))
					
				}
				
				def finalizeUpload(upload: Box[FileParamHolder]) {
					
					debug("In finalize upload")
					
					upload match {
						case Empty => {}
						case f: Full[FileParamHolder] => {
							
							//The raw holder
							val holder = f.open_!.asInstanceOf[OnDiskFileParamHolder]
							
							//Ugly as hell, but we need to re-create a context and storage layer to add the filename
							//Of the uploaded file to the resource path... So this verride the instance version
							val resource = stripTrailingSlash(this.verb.header("resource")) + "/" + holder.fileName
							val verb = new WEBSITE(Map("resource" -> resource),'prop )
							implicit val ctx = new LiftContext(verb)
							ctx.user = user
							ctx.userConfig = Config.userConfig(ctx)

							val storageClient = VMTalk getStorageClient;
							ctx.storageclient = storageClient							
							ctx.store = Storage.mkStorage(
								ctx.userConfig("storage"),
								(ctx.verb.header("resource"))
							)
							
							//Make sure the user directory exists for local filesystem
							if(ctx.userConfig("storage") == "filesystem") {
								val user_files = Config("filesystem_prefix","/tmp/") + user
								(new File(user_files)).exists match {
									//Directory exists, do nothing
									case true => user_files
									case false =>  (new File(user_files)).mkdirs()
								}
							}
							

							//Create a metatdata store, if needed
							val metadata = new MetaData()
							//The full metadata key to our file
							val fullkey = stripTrailingSlash("/" + user + verb.header("resource"))
							//We use this on error to determine if we need to delete or not
							val resource_overwrite = metadata exists(fullkey)
							debug("resource already exists (overwrite): "+resource_overwrite)
							//We need to capture this, because in case of an error we might need to reset it
							val oldguid = resource_overwrite match {
								case true => metadata getOriginal(fullkey)
								case false => ""
							}
							//Capture old metadata, in case of recovery later
							val old_metadata: Map[String,String] = resource_overwrite match {
								case true => metadata getMetaData(fullkey)
								case false => Map()
							}
							
							//Get the localFile from the Lift upload
							val localFile = holder.localFile
							//Set the actualContentLength
							ctx.actualContentLength = localFile length;
							//The new_guid is used for transferLocalFile
							//This allows us to recover to oldguid if it's an overwrite
							val new_guid = resource_overwrite match {
								//We need to generate a new guid for the new data
								case true => UUID
								//We already have a new guid, so retrieve it from the store
								case false => {
									ctx.store getOriginalName
								}
							}
							
							//Switch for local filesystem; if local, only extract the UUID part, e.g. the filename
							val new_guid_tmp_file =  (ctx.userConfig("storage")) match {
								//File system case
								case s: String if s  == "filesystem" => fileNameFromPath(ctx.store.getOriginalName)
								//Flat object space, so jus a UUID
								case _ => ctx.store getOriginalName
							}

							debug("---new_guid_tmp_file = "+new_guid_tmp_file)	
							val zipped_encrypted = File createTempFile(new_guid_tmp_file,"")
							
							//Here we create a closure for updating the metadata. This way we can re-run it when the
							//metadata store fails
							val update_metadata_fun = { () => {
									metadata setOriginal(fullkey,new_guid)
									debug("setOriginal to"+new_guid+" for "+fullkey)
									metadata setMetaData(fullkey,metadata davEncode("getcontentlanguage"),"en")
									debug("getcontentlanguage")
									metadata setMetaData(fullkey,metadata davEncode("getcontentlength"),ctx.actualContentLength toString)
									debug("getcontentlength")
									metadata setMetaData(fullkey,metadata davEncode("getcontenttype"),"application/binary")
									debug ("getcontenttype")
									metadata setMetaData(fullkey,metadata davEncode("getetag"),UUID())		
									debug ("getetag")
									//TBD only creationdate on new resource
									metadata setMetaData(fullkey,metadata davEncode("creationdate"),idateNow)
									metadata setMetaData(fullkey,metadata davEncode("displayname"),holder.fileName)
									metadata setMetaData(fullkey,metadata davEncode("getlastmodified"),idateNow)
									metadata setMetaData(fullkey,metadata davEncode("resourcetype"),"")
									metadata setMetaData(fullkey,metadata davEncode("source"),"")
									metadata setPutLength(fullkey,ctx.putContentLength)								
								}
							}
							
							//The main update
							try { 
								debug("Data uploaded, start processing")
								//Create a temp file that we'll use to compress and encrypt the file
								//Compress and encrypt
								//Note: after this point the file will be created in the metadata store
								PipeUtils in2CloudFile(new FileInputStream(localFile),zipped_encrypted)
								ctx.putContentLength = zipped_encrypted.length
								debug("File encrypted and zipped")
								//Store the file in the storage layer
								ctx.store transferLocalFile(zipped_encrypted,fileNameFromPath(new_guid))
								debug("File transfered")
								//Delete the temp file
								zipped_encrypted delete;
								//debug("temp file deleted")
								debug("zipped encrypted file on: "+ (zipped_encrypted getAbsolutePath))
								//Set the Metadata on the newly uploaded file
								debug("new guid ="+new_guid)
								debug("under key: "+fullkey+", updating metadata")
								update_metadata_fun();
								debug("metadata updated")
								if (resource_overwrite) {
									debug("Deleting old data directly im store (because it's an overwrite)")
									try {ctx.store.deleteDirectly(oldguid)} 
									//TBD: collect this data for batch cleanup
									catch {case _ => {debug("Zombie data left in store with id: "+oldguid)}}
								}
								tryo {
									//Add metering info
									val moi_now = userMonthYear(ctx.user)
									val moi_year = userYear(ctx.user)
									//Get client for storing in voldemort
									val incoming_meterer = VMTalk getIncomingMeterer;
									val incoming_data = ctx.actualContentLength
									debug("incoming_data in CloudFiles = "+incoming_data)
									//Initialize or current value
									incoming_meterer init(moi_now,0L)
									incoming_meterer init(moi_year,0L)
									//And add to the incoming meterer
									incoming_meterer applyDelta(moi_now,incoming_data,add_stale_update)
									incoming_meterer applyDelta(moi_year,incoming_data,add_stale_update)
									//incoming_meterer.factory.close;
									//Now, we're going to up the rquest counter
									val request_meterer = VMTalk getRequestMeterer;
									//Initialize or current value
									//val request_count : Int = 1 + (request_meterer getvalue moi)
									//And add to the incoming meterer
									request_meterer init(moi_now,0L)
									request_meterer init(moi_year,0L)
									request_meterer applyDelta(moi_now,1L,add_stale_update)
									request_meterer applyDelta(moi_year,1L,add_stale_update)
									//request_meterer.factory.close;
									//GBMonth storage metering
									//Distinguish based on verb, i.e. COPY, DELETE, PUT, POST, default cases...
									var hours = (hoursOfThisMonth - currentMonthHour)
									debug("****hours left this month: " + hours.toString)
									var key = userMonthYear(ctx.user);
									debug("Key = " + key)

									//We only do this so that the actual key/value for this month is initialized
									val month: Versioned[GBMonth] = hours match {

										case 0  => {
											debug("No more hours left this month, get hours of next month")
											//We are at the end of the month, so or #hours is the hours of next month
											//Key needs to change, tpoo
											hours = hoursOfNextMonth
											debug("Updating key to next month")
											key = nextUserMonthYear(ctx.user);
											debug("...and initalizing next month")
											initNextGBMonth(ctx.user)
										}

										case _ => {
											debug("hours left, get the current GBMonth")
											getGBMonth(ctx.user)
										}
									}
									val path = stripTrailingSlash("/" + ctx.user + ctx.verb.header("resource"))
									val storage_length = sizeOfPath(path,ctx,metadata)
									//This way, we correct for oerwrotes of data accidentally doubling usage
									//storage_length is negatve as per the || then
									val length = ctx.putContentLength //+ storage_length
									val delta: (Long,Long) = (length,(length * hours))
									debug("POST, delta = " + delta)
									gbmonth_meterer applyDelta(key,delta,add_gbmonth_update)
									
								}

							} catch {
								//The upload failed
								case e: AmazonClientException  => {
									debug("amazon exception on transfer")
									resource_overwrite match {

										case true => {
											//The upload failed, so the old data is probably still in s3
											//Delete the local file
											tryo {zipped_encrypted delete}
											//Reset the original pointer to the oldguid!
											metadata setOriginal(fullkey,oldguid)
											S.error(S ? "Upload to back end failed, please try again or contact the help desk. Your old data has been preserved.")
										}
										
										case false => {	
											//Cleanup, everything went wrong on a new upload. Report the error
											if (metadata exists fullkey) {metadata delete fullkey}
											tryo {zipped_encrypted delete}
											tryo {ctx.store.delete}
											//And notify the user
											S.error(S ? "Upload to back end failed, please try agaon or contact the help desk.")
										}
									}
								}
								
								//In this case, we probably have the data transfered, but failed on the metadata
								//So we need to use the  closure and replay it again, otherwise regard it as failed
								case e: Exception => {
									e.printStackTrace
									debug("failed to update metadata")
									//Try again, but revert/delete on repetive failure
									try {update_metadata_fun()} catch {
										//On error we haven't been able to update the metadata
										case _ => {
											resource_overwrite match {
												//But we had a resource already. Try to revert all metadata fields.
												case true => {
													//Revert original
													tryo {metadata setOriginal(fullkey,oldguid)}
													//Try and set the old metadata back
													tryo {metadata setMetaData(fullkey,old_metadata)}
													//Cleanup, everything went wrong
													tryo {zipped_encrypted delete}
													//Delete the new version
													tryo {ctx.store.deleteDirectly(new_guid)}
													//And notify the user
													S.error(S ? "Upload to back end failed, please try again or contact the help desk. Your old data has been preserved.")
												}
												//We had no resource, so we bluntly delete the data and report it to the user
												case false => {
													if (metadata exists fullkey) {tryo {metadata delete fullkey}}
													tryo {zipped_encrypted delete}
													tryo {ctx.store.delete}
													//And notify the user
													S.error(S ? "Upload to back end failed, please try agaon or contact the help desk.")													
												}
												
											}
											
										 } 
									}
								}
							}
							
						}
					}
				}
				
				def hasTags(tags: List[String]) = {
					//Search for the tags
					metadata.treeAsList("/" + user).filterNot(x => (metadata.isCollection(x) || metadata.isLink(x))).filter(x => {metadata.containsTags(x,tags)})
					
				}
				
				def isTagFolder(): Boolean = {
					metadata.isTagFolder(fullkey)
				}
				
				def isTagFolder_?(path: String): Boolean = {
					val fullkey = "/"+ctx.user+"/"+stripLeadingSlash(path)
					debug("In CloudFiles isTagFolder_?, fullkey = "+fullkey)
					val res = metadata.isTagFolder_?(fullkey)
					debug("result = "+res)
					res
				}			
				def inTagFolder(): Boolean = {metadata.inTagFolder(fullkey)}
				
				def isMandatoryFolder(path: String): Boolean = {
					//First, a full expansion
					val fullpath = "/" + ctx.user + "/" + stripLeadingSlash(path)
					((fullpath == ("/" + ctx.user + "/" + Config("tag_folder_root",""))) || (fullpath == ("/" + ctx.user + "/" +Config("common_folder_root",""))) || (fullpath == ("/" + ctx.user + "/" + Config("public_folder_root"))))
				}
				
				def saveTagFolder(foldername: String,files: List[String],tags: List[String]) {
					
					debug("in CloudFiles.saveTagFolder")
					debug("foldername = "+ foldername)
					debug("tags are : "+tags)
					debug("files are: "+files)
					val fullpath = "/" + user + "/" + stripLeadingSlash(Config("tag_folder_root")) +"/" + stripLeadingSlash(foldername)
					debug("fullpath = "+ fullpath)
					metadata.createTagFolder(fullpath,files,tags,Map())
				}			
			
				override def finalize() {
					//gbmonth_meterer.factory.close;
				}
			
			}			
		}
	}
}