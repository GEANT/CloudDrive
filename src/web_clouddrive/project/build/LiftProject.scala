import sbt._

import java.util.jar.{Manifest,Attributes}
import java.io.File
import sbt.FileUtilities._
import Attributes.Name.CLASS_PATH
import java.util.jar.Attributes.Name._
import java.lang.String
import java.io.PrintWriter
import scala.collection.mutable
import scala.io.Source


class LiftProject(info: ProjectInfo) extends DefaultWebProject(info)   {


  val liftVersion = property[Version]
  //val mavenLocal = "Local Maven Repository" at
  //"file://"+Path.userHome+"/.m2/repository"

  //val scalatools_snapshot = "Scala Tools Snapshot" at
  //"http://scala-tools.org/repo-snapshots/"
  lazy val JavaNet = "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

  val scalatools_release = "Scala Tools Snapshot" at
  "http://scala-tools.org/repo-releasesi/"
	
  override def compileOptions = super.compileOptions ++ Seq(Optimize)

  // uncomment the following if you want to use the snapshot repo
  val scalatoolsSnapshot = ScalaToolsSnapshots

  // If you're using JRebel for Lift development, uncomment
  // this line
  // override def scanDirectories = Nil

  //val liftVersion = "2.3"
  val mysql = "mysql" % "mysql-connector-java" % "5.1.12"

  override def libraryDependencies = Set(
    "net.liftweb" %% "lift-webkit" % liftVersion.value.toString % "compile",
    "net.liftweb" %% "lift-mapper" % liftVersion.value.toString % "compile",
	"net.liftweb" %% "lift-wizard" % liftVersion.value.toString % "compile",
	"net.liftweb" %% "lift-widgets" % liftVersion.value.toString % "compile",
	"org.mortbay.jetty" % "jetty" % "6.1.22" % "compile,test"
	//"junit" % "junit" % "4.7" % "test",
    //"ch.qos.logback" % "logback-classic" % "0.9.26",
    //"org.scala-tools.testing" %% "specs" % "1.6.8" % "test",
    //"com.h2database" % "h2" % "1.2.147"
  ) ++ super.libraryDependencies


}

