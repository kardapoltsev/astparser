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

package com.github.kardapoltsev.astparser

import com.github.kardapoltsev.astparser.model.Model
import com.github.kardapoltsev.astparser.parser.{AstParser, Model => ParsedModel}
import org.scalatest.{Matchers, WordSpec}

trait TestBase extends WordSpec with Matchers {
  protected val fileExt = "sl"
  protected val parser  = new AstParser()

  protected def buildParserModel(source: String, sources: String*): ParsedModel = {
    val schemas = (source +: sources).zipWithIndex map {
      case (src, idx) =>
        parser.parse(src, "test_source_" + idx)
    }

    ParsedModel(schemas)
  }

  protected def buildModel(sources: String*): Model = {
    val schemas = sources.zipWithIndex map {
      case (src, idx) =>
        src -> ("test_source_" + idx)
    }

    Model.buildFromString(schemas)
  }

}
