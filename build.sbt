organization := "de.dfki.cps"
name := "secore"
scalaVersion := "2.11.8"
version := "1.0.2"
licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
bintrayOrganization := Some("dfki-cps")

scalacOptions := Seq("-deprecation")

crossScalaVersions := Seq("2.11.8","2.12.1")

libraryDependencies += "de.dfki.cps" % "specific-dependencies" % "4.6.3"
libraryDependencies += "de.dfki.cps" %% "stools" % "1.1.0"

libraryDependencies += "de.dfki.cps" %% "specific-sysml" % "0.1.12" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"