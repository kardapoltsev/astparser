/*
  Copyright 2016 Alexey Kardapoltsev

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/

import sbt.Keys._
import sbt._



object AstParserBuild extends Build {
  val Organization = "com.github.kardapoltsev"
  val ScalaVersion = "2.11.8"
  val SkipUpdate   = true
  val CacheUpdate  = true
  val isSnapshot   = true
  val baseVersion: String = "1.1.1"

  val appVersion = {
    if(isSnapshot) baseVersion + "-SNAPSHOT"
    else baseVersion
  }


  lazy val buildSettings =
      Seq(
        organization         := Organization,
        version              := appVersion,
        scalaVersion         := ScalaVersion,
        //scoverage isn't available for 2.12
        //crossScalaVersions   := Seq("2.10.6", ScalaVersion, "2.12.0-RC1"),
        crossScalaVersions   := Seq("2.10.6", ScalaVersion),
        organizationName     := Organization,
        organizationHomepage := Some(url("https://github.com/kardapoltsev")),
        parallelExecution in Test := true,

        initialize                ~= { _ =>
          if (sys.props("java.specification.version") < "1.7")
            sys.error("Java 7 is required for this project.")
        },
        updateOptions := updateOptions.value.withCachedResolution(CacheUpdate),
        incOptions := incOptions.value.withNameHashing(true),
        scalacOptions ++= Seq(
          "-encoding",
          "UTF-8",
          "-deprecation",
          "-unchecked",
          "-feature",
          "-Xlint"
        ),
        scalacOptions <++= scalaVersion map { sv =>
          if(sv.startsWith("2.10")) {
            Seq.empty[String]
          } else {
            Seq(
              "-Ywarn-unused-import"
            )
          }
        }
      )

  val publishSettings = Seq(
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
  )


  import de.heikoseeberger.sbtheader.HeaderPattern
  import de.heikoseeberger.sbtheader.AutomateHeaderPlugin
  import de.heikoseeberger.sbtheader.HeaderKey.headers
  private val ScalaHeader =
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

  lazy val scalaHeaderSettings = Seq(
    headers := Map(
      "scala" -> ( HeaderPattern.cStyleBlockComment, ScalaHeader )
    )
  )


  lazy val root = Project(
    "ast-parser",
    file("."),
    settings =
      buildSettings ++ scalaHeaderSettings ++ publishSettings ++
        Seq(
          scalacOptions in (Compile,doc) :=  Seq(
            "-encoding",
            "UTF-8",
            "-Xlint",
            "-deprecation",
            "-unchecked",
            "-groups",
            "-implicits",
            "-diagrams"
          ),
          libraryDependencies <++= scalaVersion { sv =>
            if(sv.startsWith("2.10"))
              Dependencies.rootScala210
            else
              Dependencies.root
          }
        )
  ).enablePlugins(AutomateHeaderPlugin)

}
