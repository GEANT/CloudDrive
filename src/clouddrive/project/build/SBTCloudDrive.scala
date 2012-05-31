import sbt._

class SBTCloudDrive(info: ProjectInfo) extends DefaultProject(info)  with assembly.AssemblyBuilder {
	
	override def compileOptions = super.compileOptions ++ Seq(Optimize)
	
	lazy val JavaNet = "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"
	
	override def mainClass = Some("net.vrijheid.clouddrive.MainServerStart")
	
}