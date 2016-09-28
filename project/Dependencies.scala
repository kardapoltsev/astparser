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

import sbt._

object Dependencies {
  object V {
  }

  object Compile {
    val logbackClassic  = "ch.qos.logback"                % "logback-classic"                % "1.1.6"
    val scalaParsers    = "org.scala-lang.modules"        %% "scala-parser-combinators"      % "1.0.4"
  }

  object Test {
    val scalatest         = "org.scalatest"              %% "scalatest"                     % "2.2.5"           % "test"
  }

  import Compile._
  import Test._

  val root = Seq(
    logbackClassic, scalaParsers, scalatest
  )
  val rootScala210 = Seq(
    logbackClassic, scalatest
  )

}
