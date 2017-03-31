val Organization = "com.github.kardapoltsev"
val SkipUpdate   = true
val CacheUpdate  = true

organization         := Organization
name := "ast-parser"
scalaVersion         := "2.11.8"
crossScalaVersions   := Seq("2.10.6", scalaVersion.value, "2.12.1")
organizationName     := Organization
organizationHomepage := Some(url("https://github.com/kardapoltsev"))
parallelExecution in Test := true

initialize                ~= { _ =>
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
  "-Xlint"
)
scalacOptions <++= scalaVersion map { sv =>
  if(sv.startsWith("2.10")) {
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


import de.heikoseeberger.sbtheader.HeaderPattern
import de.heikoseeberger.sbtheader.AutomateHeaderPlugin
import de.heikoseeberger.sbtheader.HeaderKey.headers
val ScalaHeader =
  """/*
    |  Copyright 2016 Alexey Kardapoltsev
    |
    |  Licensed under the Apache License, Version 2.0 (the "License");
    |  you may not use this file except in compliance with the License.
    |  You may obtain a copy of the License at
    |
    |      http://www.apache.org/licenses/LICENSE-2.0
    |
    |  Unless required by applicable law or agreed to in writing, software
    |  distributed under the License is distributed on an "AS IS" BASIS,
    |  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    |  See the License for the specific language governing permissions and
    |  limitations under the License.
    |*/
    |""".stripMargin

headers := Map(
  "scala" -> ( HeaderPattern.cStyleBlockComment, ScalaHeader )
)

val slf4jApi        = "org.slf4j"                    %  "slf4j-api"                     % "1.7.25"
val scalaParsers    = "org.scala-lang.modules"       %% "scala-parser-combinators"      % "1.0.4"
val logbackClassic  = "ch.qos.logback"               %  "logback-classic"               % "1.2.3"         % "test"
val scalatest       = "org.scalatest"                %% "scalatest"                     % "3.0.1"         % "test"

val baseDependencies = Seq(
  slf4jApi, logbackClassic, scalaParsers, scalatest
)
val scala210Dependencies = Seq(
  slf4jApi, logbackClassic, scalatest
)

scalacOptions in (Compile,doc) :=  Seq(
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
  if(sv.startsWith("2.10"))
    scala210Dependencies
  else
    baseDependencies
}

enablePlugins(AutomateHeaderPlugin)
