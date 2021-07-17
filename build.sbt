val Organization = "com.github.kardapoltsev"

organization := Organization
name := "ast-parser"
scalaVersion := "2.13.4"
crossScalaVersions := Seq("2.11.12", "2.12.11", scalaVersion.value)
organizationName := "Alexey Kardapoltsev"
organizationHomepage := Some(url("https://github.com/kardapoltsev"))
Test / parallelExecution := true
ThisBuild / scalafmtOnCompile := true
Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / scapegoatVersion := "1.4.8"

initialize ~= { _ =>
  if (sys.props("java.specification.version") < "1.7")
    sys.error("Java 7 is required for this project.")
}

addCompilerPlugin(scalafixSemanticdb)
addCommandAlias("checkall", "; compile:scalafix --check ; test:scalafix --check")
addCommandAlias("fixall", "all compile:scalafix test:scalafix")

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  //"-Xfatal-warnings", turned of because of StringLike.lineIterator/lines deprecations
  "-Xlint",
  "-Yrangepos"
)

scalacOptions ++= {
  if (scalaVersion.value.startsWith("2.10") || scalaVersion.value.startsWith("2.13")) {
    Seq.empty[String]
  } else {
    Seq(
      "-Ywarn-unused-import"
    )
  }
}

//sbt-release configuration
releasePublishArtifactsAction := PgpKeys.publishSigned.value
releaseCrossBuild := true

//publish configuration
publishMavenStyle := true
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)
homepage := Some(url("https://github.com/kardapoltsev/astparser"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/kardapoltsev/astparser"),
    "scm:git@github.com:kardapoltsev/astparser.git"
  )
)
developers := List(
  Developer(
    id = "kardapoltsev",
    name = "Alexey Kardapoltsev",
    email = "alexey.kardapoltsev@gmail.com",
    url = url("https://github.com/kardapoltsev")
  )
)
licenses := Seq(("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")))

import de.heikoseeberger.sbtheader.AutomateHeaderPlugin
startYear := Some(2016)

val scalaParsers  = "org.scala-lang.modules"   %% "scala-parser-combinators" % "1.1.2"
val scalatest     = "org.scalatest"            %% "scalatest"                % "3.2.9"  % "test"
val log4jApi      = "org.apache.logging.log4j"  % "log4j-api"                % "2.14.1"
val log4jCore     = "org.apache.logging.log4j"  % "log4j-core"               % "2.14.1" % "test"
val log4jScalaApi = "org.apache.logging.log4j" %% "log4j-api-scala"          % "12.0"

val baseDependencies = Seq(
  log4jApi,
  log4jCore,
  log4jScalaApi,
  scalaParsers,
  scalatest
)

Compile / doc / scalacOptions := Seq(
  "-encoding",
  "UTF-8",
  "-Xlint",
  "-deprecation",
  "-unchecked",
  "-groups",
  "-implicits",
  "-diagrams"
)

libraryDependencies ++= baseDependencies

enablePlugins(AutomateHeaderPlugin)
