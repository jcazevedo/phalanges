import sbt._
import Keys._

import sbtassembly.Plugin._
import AssemblyKeys._

object ProjectBuild extends Build {
  lazy val project = "phalange"

  lazy val root = Project(id = project,
                          base = file("."),
                          settings = Project.defaultSettings ++ assemblySettings).settings(
    organization := "net.jcazevedo",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.9.2",

    resolvers ++= Seq(
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
      "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/",
      "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
      "releases" at "http://oss.sonatype.org/content/repositories/releases"
    ),

    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "1.11" % "test"
    ),

    scalacOptions ++= Seq("-deprecation", "-unchecked")
  )
}
