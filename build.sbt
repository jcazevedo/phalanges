import sbt.CrossVersion
import sbt.Keys.scalaVersion

name := "phalange"

organization := "net.jcazevedo"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.10"

crossScalaVersions := Seq("2.12.17", "2.13.10")

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.8.0" % "test")

scalacOptions ++= {
  val allVersionFlags = List(
    "-encoding",
    "UTF-8", // yes, this is 2 args
    "-feature",
    "-unchecked",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen"
  )

  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) =>
      allVersionFlags ++ List(
        "-deprecation",
        "-Xlint:_,-unused",
        "-Xfatal-warnings",
        "-Yno-adapted-args",
        "-Ywarn-unused:_,-implicits"
      )

    case Some((2, 13)) =>
      allVersionFlags ++ List("-Ywarn-unused:_,-implicits")

    case _ =>
      allVersionFlags
  }
}

Compile / console / scalacOptions --= Seq("-Xfatal-warnings", "-Ywarn-unused-import", "-Ywarn-unused:_,-implicits")
Test / console / scalacOptions := (Compile / console / scalacOptions).value

publishMavenStyle := true

publishTo              := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

Test / publishArtifact := false

pomIncludeRepository := { _ => false }

licenses := Seq(
  "MIT License" ->
    url("http://www.opensource.org/licenses/mit-license.php")
)

homepage := Some(url("https://github.com/jcazevedo/phalange"))

pomExtra := (<scm>
    <url>git@github.com:jcazevedo/phalange.git</url>
    <connection>scm:git:git@github.com:jcazevedo/phalange.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jcazevedo</id>
      <name>Joao Azevedo</name>
      <url>http://jcazevedo.net</url>
    </developer>
  </developers>)
