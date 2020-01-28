val Organization = "com.github.kardapoltsev"
val SkipUpdate   = true
val CacheUpdate  = true

organization := Organization
name := "ast-parser"
scalaVersion := "2.11.12"
crossScalaVersions := Seq(scalaVersion.value, "2.12.10") // 2.10 was removed due to scalafix plugin
organizationName := "Alexey Kardapoltsev"
organizationHomepage := Some(url("https://github.com/kardapoltsev"))
parallelExecution in Test := true
scalafmtOnCompile in ThisBuild := true
Global / onChangedBuildSource := ReloadOnSourceChanges

initialize ~= { _ =>
  if (sys.props("java.specification.version") < "1.7")
    sys.error("Java 7 is required for this project.")
}
updateOptions := updateOptions.value.withCachedResolution(CacheUpdate)
//incOptions := incOptions.value.withNameHashing(true)

scalafixDependencies in ThisBuild += "org.scalatest" %% "autofix" % "3.1.0.0"
addCompilerPlugin(scalafixSemanticdb)
addCommandAlias("checkall", "; compile:scalafix --check ; test:scalafix --check")
addCommandAlias("fixall", "all compile:scalafix test:scalafix")

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
//  "-Xfatal-warnings",
  "-Ywarn-unused-import",
  "-Xlint",
  "-Yrangepos"
)

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
    url = url("https://github.com/kardapoltsev"))
)
licenses := Seq(("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")))

import de.heikoseeberger.sbtheader.AutomateHeaderPlugin
startYear := Some(2016)

val scalaParsers  = "org.scala-lang.modules"   %% "scala-parser-combinators" % "1.1.2"
val scalatest     = "org.scalatest"            %% "scalatest"                % "3.1.0" % "test"
val log4jApi      = "org.apache.logging.log4j" % "log4j-api"                 % "2.13.0"
val log4jCore     = "org.apache.logging.log4j" % "log4j-core"                % "2.13.0" % "test"
val log4jScalaApi = "org.apache.logging.log4j" %% "log4j-api-scala"          % "11.0"

val baseDependencies = Seq(
  log4jApi,
  log4jCore,
  log4jScalaApi,
  scalaParsers,
  scalatest
)

scalacOptions in (Compile, doc) := Seq(
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
