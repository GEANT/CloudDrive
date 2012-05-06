import java.io._
import java.util._
import net.vrijheid.clouddrive.client._

package net.vrijheid.clouddrive.test.client {


	class LoadClient(hostname: String, user: String, password: String,ssl: Boolean) extends SimpleClient(hostname,user, password,ssl) with Runnable {
	
		private var in : String = _
		private val name: String = (java.util.UUID.randomUUID()).toString
				
		def setSource(in: String) { this.in = in }
	
		def run() {
			debug("Thread name "+name+" running.")
			//Execute the test
			//MKCOL, PUT, GET, DELETE, DEL Collection
			try 
			{
				makeCollection("/" + name.toString)
				putFile(in,"/"+name.toString+"/"+in)
				getFile("/tmp/"+name.toString+"_"+in,"/"+name.toString+"/"+in)
				deleteFile("/"+name.toString+"/"+in)
				deleteCollection("/" + name.toString)			
			} 
			catch  
			{
				case e : Exception => {
					debug("***Thread name "+name+" :")
					e printStackTrace
				}
			}

			
			
		}
	
	}

}

object LoadServerStart {
	def main(args: Array[String]) {
		
		try {
			//Launch code:
			//read properties file
			val config = (new Properties)
			config.load(new FileInputStream(new File(args(0))))
			val host = config getProperty("hostname")
			val ssl = if (config.getProperty("ssl") == "true") {true} else {false}
			val user = config getProperty ("user")
			val password = config getProperty ("password")
			val input = config getProperty ("input")
			val how_many = (1 to (config.getProperty("load").toInt)).toList
			//Create  # instances with same source file, put in list
			//Genereate the clients 
			val clients = how_many.map {
				(x) => { 
						val client = new net.vrijheid.clouddrive.test.client.LoadClient(host,user,password,ssl)
						client setSource input
						client	
					}
			}
			
			println (clients)
			
			clients.foreach{ client => {(new Thread(client)).start()}}
			
		} catch {
			case e: Exception => {
				System.out.println("usage: java -jar clouddrive.jar net.vrijheid.test.client.LoadServerStart config-file")
			}
		}
		
	}
}