import net.vrijheid.clouddrive.pipes._
import net.vrijheid.clouddrive.control._
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.httpsupport._
import net.vrijheid.clouddrive.providers._
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes.webdavcmds {
	
	//
	class OPTIONSSink[T](implicit val ctx: RootContext[T])  extends MetaData[T] with PipeSink  with Serializable {
		
		private var header : String = ""
		
		
		override def ||() {
			
			debug("Initializing OPTIONS command handler")
			
			val resource = ctx.verb.header.getOrElse("resource","/")
			resource match {
				case "/"  => {
					
					val header_fields = Map (
						"DAV" -> "1,2",
						"MS-Author-Via" -> "DAV",
						//LOCK, UNLOCK here for CLASS2
						"ALLOW" -> "OPTIONS, GET, DELETE, PUT,POST, MKCOL, MOVE, COPY, PROPFIND, PROPPATCH, HEAD, LOCK, UNLOCK"
					)
					this header = HTTPServerHelper.httpHeader (200,header_fields,0)
				}
				
				case "*"  => {
					val header_fields = Map (
						"Dav" -> "1",
						"MS-Author-Via" -> "DAV",
						//LOCK, UNLOCK here for CLASS2
						"ALLOW" -> "LOCK, UNLOCK, OPTIONS, GET, DELETE, PUT,POST, MKCOL, MOVE, COPY, PROPFIND, PROPPATCH, HEAD"
					)
					this header = HTTPServerHelper.httpHeader (200,header_fields,0)
				}
				
				case s : String => {
					//Create the fullpath in theuser namespace
					val fullpath = followLink("/" + ctx.user + s)
					exists(fullpath) match {
						
						case false  => {
							this header = HTTPServerHelper.httpHeader(404,Map[String,String](),0)
						}
						
						case true => {
							val header_fields = Map (
								"Dav" -> "1",
								"MS-Author-Via" -> "DAV",
								//LOCK, UNLOCK here for CLASS2
								"Allow" -> "LOCK, UNLOCK, OPTIONS, GET, DELETE, PUT,POST, MKCOL, MOVE, COPY, PROPFIND, PROPPATCH, HEAD"
							)
							this header = HTTPServerHelper.httpHeader (200,header_fields,0)							
						}
					}
				}
			}
			
		}
		
		override def <| () = {
			debug("OPTIONS Command Sink <|: " + header)
			ctx.phase = 'allwritten
			header getBytes
		}
	}
	
}