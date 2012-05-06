import sbt._

class SBTCloudDrive(info: ProjectInfo) extends DefaultProject(info)  with assembly.AssemblyBuilder {
	
	override def compileOptions = super.compileOptions ++ Seq(Optimize)
	
	lazy val JavaNet = "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"
	
	lazy val copyConf = task {
		
		log.info("Copy conf task...")
		val confsrc = "src" / "main" / "scala" / "config.txt"
		val conftarget = "." / "config.txt"
		FileUtilities.copyFile(confsrc.asFile,conftarget.asFile,new ConsoleLogger)
	}

	
	override def compileAction = super.compileAction dependsOn(copyConf)
	override def mainClass = Some("net.vrijheid.clouddrive.MainServerStart")
	
}