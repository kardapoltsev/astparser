val Organization = "com.github.kardapoltsev"
val SkipUpdate   = true
val CacheUpdate  = true

organization := Organization
name := "ast-parser"
scalaVersion := "2.11.12"
crossScalaVersions := Seq("2.10.7", scalaVersion.value, "2.12.6")
organizationName := "Alexey Kardapoltsev"
organizationHomepage := Some(url("https://github.com/kardapoltsev"))
parallelExecution in Test := true
scalafmtOnCompile in ThisBuild := true

initialize ~= { _ =>
  if (sys.props("java.specification.version") < "1.7")
    sys.error("Java 7 is required for this project.")
}
updateOptions := updateOptions.value.withCachedResolution(CacheUpdate)
//incOptions := incOptions.value.withNameHashing(true)
scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Xfatal-warnings",
  "-Xlint"
)
scalacOptions ++= {
  if (scalaVersion.value.startsWith("2.10")) {
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
    url = url("https://github.com/kardapoltsev"))
)
licenses := Seq(("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")))

import de.heikoseeberger.sbtheader.AutomateHeaderPlugin
startYear := Some(2016)

val scalaParsers  = "org.scala-lang.modules"   %% "scala-parser-combinators" % "1.1.0"
val scalatest     = "org.scalatest"            %% "scalatest"                % "3.0.5" % "test"
val log4jApi      = "org.apache.logging.log4j" % "log4j-api"                 % "2.11.0"
val log4jCore     = "org.apache.logging.log4j" % "log4j-core"                % "2.11.0" % "test"
val log4jScalaApi = "org.apache.logging.log4j" %% "log4j-api-scala"          % "11.0"

val baseDependencies = Seq(
  log4jApi,
  log4jCore,
  log4jScalaApi,
  scalaParsers,
  scalatest
)
val scala210Dependencies = Seq(
  log4jApi,
  log4jCore,
  log4jScalaApi,
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
libraryDependencies ++= {
  if (scalaVersion.value.startsWith("2.10"))
    scala210Dependencies
  else
    baseDependencies
}

enablePlugins(AutomateHeaderPlugin)
