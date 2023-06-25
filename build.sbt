val Organization = "com.github.kardapoltsev"

organization                  := Organization
name                          := "ast-parser"
scalaVersion                  := "2.13.11"
crossScalaVersions            := Seq("2.12.18", scalaVersion.value)
organizationName              := "Alexey Kardapoltsev"
organizationHomepage          := Some(url("https://github.com/kardapoltsev"))
Test / parallelExecution      := true
ThisBuild / scalafmtOnCompile := true
Global / onChangedBuildSource := ReloadOnSourceChanges

addCompilerPlugin(scalafixSemanticdb)
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Xfatal-warnings", //turned of because of StringLike.lineIterator/lines deprecations
  "-Xlint",
  "-Yrangepos",
  "-Xsource:3",
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
releaseCrossBuild             := true

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

val scalaParsers  = "org.scala-lang.modules"   %% "scala-parser-combinators" % "2.2.0"
val scalatest     = "org.scalatest"            %% "scalatest"                % "3.2.15" % "test"
val log4jApi      = "org.apache.logging.log4j"  % "log4j-api"                % "2.20.0"
val log4jCore     = "org.apache.logging.log4j"  % "log4j-core"               % "2.20.0" % "test"
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
