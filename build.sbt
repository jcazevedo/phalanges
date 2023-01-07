ThisBuild / scalaVersion         := "2.13.10"
ThisBuild / version              := "0.1.0-SNAPSHOT"
ThisBuild / crossScalaVersions   := Seq("2.12.17", "2.13.10")
ThisBuild / organization         := "net.jcazevedo"
ThisBuild / organizationName     := "jcazevedo"
ThisBuild / organizationHomepage := Some(url("https://jcazevedo.net/"))

ThisBuild / scmInfo    := Some(
  ScmInfo(
    url("https://github.com/jcazevedo/phalanges"),
    "scm:git@github.com:jcazevedo/phalanges.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "jcazevedo",
    name = "Joao Azevedo",
    email = "joao.c.azevedo@gmail.com",
    url = url("https://jcazevedo.net")
  )
)

ThisBuild / licenses   := Seq(
  "MIT License" ->
    url("http://www.opensource.org/licenses/mit-license.php")
)
ThisBuild / homepage   := Some(url("https://github.com/jcazevedo/phalanges"))

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo            := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle    := true

// Enable SemanticDB for Scalafix.
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

// Enable the OrganizeImports Scalafix rule.
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

ThisBuild / scalacOptions ++= {
  val allVersionFlags = List(
    "-encoding",
    "UTF-8",
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

lazy val core = (project in file("modules/core")).settings(
  name                           := "phalanges-core",
  description                    := "An implementation of finger trees as proposed by Hinze and Paterson.",
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-collection-compat" % "2.8.1",
    "org.scalacheck"         %% "scalacheck"              % "1.17.0" % "test",
    "org.specs2"             %% "specs2-core"             % "4.17.0" % "test",
    "org.specs2"             %% "specs2-scalacheck"       % "4.17.0" % "test"
  ),
  Compile / console / scalacOptions --= Seq("-Xfatal-warnings", "-Ywarn-unused-import", "-Ywarn-unused:_,-implicits"),
  Test / console / scalacOptions := (Compile / console / scalacOptions).value
)

lazy val phalanges = (project in file("."))
  .dependsOn(core)
  .settings(
    name        := "phalanges",
    description := "An implementation of finger trees as proposed by Hinze and Paterson."
  )
