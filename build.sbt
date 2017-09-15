val Organization = "com.github.kardapoltsev"
val SkipUpdate   = true
val CacheUpdate  = true

organization := Organization
name := "ast-parser"
scalaVersion := "2.11.11"
crossScalaVersions := Seq("2.10.6", scalaVersion.value, "2.12.3")
organizationName := "Alexey Kardapoltsev"
organizationHomepage := Some(url("https://github.com/kardapoltsev"))
parallelExecution in Test := true
scalafmtOnCompile in ThisBuild := true

initialize ~= { _ =>
  if (sys.props("java.specification.version") < "1.7")
    sys.error("Java 7 is required for this project.")
}
updateOptions := updateOptions.value.withCachedResolution(CacheUpdate)
incOptions := incOptions.value.withNameHashing(true)
scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Xfatal-warnings",
  "-Xlint"
)
scalacOptions <++= scalaVersion map { sv =>
  if (sv.startsWith("2.10")) {
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
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

pomExtra := {
  <url>https://github.com/kardapoltsev/astparser</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:git@github.com:kardapoltsev/astparser.git</connection>
      <url>github.com/kardapoltsev/astparser</url>
    </scm>
    <developers>
      <developer>
        <name>Alexey Kardapoltsev</name>
        <url>https://github.com/kardapoltsev</url>
        <email>alexey.kardapoltsev@gmail.com</email>
      </developer>
    </developers>
}

import de.heikoseeberger.sbtheader.AutomateHeaderPlugin
startYear := Some(2016)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))

val slf4jApi       = "org.slf4j"              % "slf4j-api"                 % "1.7.25"
val scalaParsers   = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
val logbackClassic = "ch.qos.logback"         % "logback-classic"           % "1.2.3" % "test"
val scalatest      = "org.scalatest"          %% "scalatest"                % "3.0.4" % "test"

val baseDependencies = Seq(
  slf4jApi,
  logbackClassic,
  scalaParsers,
  scalatest
)
val scala210Dependencies = Seq(
  slf4jApi,
  logbackClassic,
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
libraryDependencies <++= scalaVersion { sv =>
  if (sv.startsWith("2.10"))
    scala210Dependencies
  else
    baseDependencies
}

enablePlugins(AutomateHeaderPlugin)
