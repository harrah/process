import sbt._

class ProcessProject(info: ProjectInfo) extends DefaultProject(info)
{
	val sc = "org.scala-tools.testing" % "scalacheck" % "1.5" % "test"
	val sp = "org.scala-tools.testing" % "specs" % "1.6.0" % "test"

	override def managedStyle = ManagedStyle.Maven
	val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
	Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}