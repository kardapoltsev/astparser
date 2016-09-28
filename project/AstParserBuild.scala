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
  val SkipUpdate = true
  val CacheUpdate = true
  val appVersion: String = "1.0.2"


  lazy val buildSettings =
      Seq(
        organization         := Organization,
        version              := appVersion,
        scalaVersion         := ScalaVersion,
        crossScalaVersions   := Seq("2.10.6", ScalaVersion),
        organizationName     := Organization,
        organizationHomepage := None
      )


  lazy val commonSettings = buildSettings ++
      Seq(
        updateOptions := updateOptions.value.withCachedResolution(CacheUpdate),
        incOptions := incOptions.value.withNameHashing(true),
        scalacOptions ++= Seq(
          "-encoding",
          "UTF-8",
          "-deprecation",
          "-unchecked",
          "-feature",
          "-Xlint",
          "-language:higherKinds",
          "-language:postfixOps"
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

  lazy val defaultSettings =
    commonSettings ++
      Seq(
        initialize                ~= { _ =>
          if (sys.props("java.specification.version") < "1.7")
            sys.error("Java 7 is required for this project.")
        },
        headers := Map(
          "scala" -> ( HeaderPattern.cStyleBlockComment, ScalaHeader )
        )
      )


  lazy val root = Project(
    "ast-parser",
    file("."),
    settings =
      defaultSettings               ++
      Seq(
        publishTo := {
          val nexus = "http://nexus.local/nexus/content/repositories/"
          if (isSnapshot.value )
            Some("snapshots" at nexus + "snapshots")
          else
            Some("releases"  at nexus + "releases")
        },
        credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
        scalacOptions        in (Compile,doc)     :=  Seq("-groups", "-implicits", "-diagrams"),
        libraryDependencies <++= (scalaVersion) { sv =>
          if(sv.startsWith("2.10"))
            Dependencies.rootScala210
          else
            Dependencies.root
        }
      )
  ).enablePlugins(AutomateHeaderPlugin)

}
