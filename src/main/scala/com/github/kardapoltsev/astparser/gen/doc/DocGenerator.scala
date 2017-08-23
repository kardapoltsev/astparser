/*
 * Copyright 2016 Alexey Kardapoltsev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.kardapoltsev.astparser.gen.doc

import com.github.kardapoltsev.astparser.gen.Generator
import com.github.kardapoltsev.astparser.model._

trait DocGenerator extends Generator {

  def printDocsCoverage(schema: Schema): Unit = {
    println(s"${schema.fullName} coverage:")
    calculateDocsCoverage(schema) map (c => "\t" + c) foreach println
  }

  def calculateDocsCoverage(schema: Schema): Seq[String] = {
    val allDefinitions       = schema.deepDefinitions
    val types                = allDefinitions.collect { case t: Type => t }
    val constructors         = types.flatMap(_.constructors)
    val constructorArguments = constructors.flatMap(_.arguments)
    val calls                = allDefinitions.collect { case m: Call => m }
    val callsParams          = calls.flatMap(_.arguments)

    Seq(
      coverage("types", types),
      coverage("constructors", constructors),
      coverage("constructors arguments", constructorArguments),
      coverage("calls", calls),
      coverage("calls params", callsParams)
    )
  }

  private def coverage(label: String, definitions: Seq[Documented]): String = {
    val documented = definitions.count(_.docs.content.nonEmpty)
    val coverage =
      if (definitions.nonEmpty)
        documented.toDouble / definitions.size * 100
      else 0
    f"$label coverage: $coverage%.1f%%"
  }

}
