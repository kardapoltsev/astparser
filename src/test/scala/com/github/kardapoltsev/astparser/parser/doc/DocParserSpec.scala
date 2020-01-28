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

package com.github.kardapoltsev.astparser.parser.doc

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait DocParserEnv extends DocParser {

  protected def parse[T](p: Parser[T], input: CharSequence): T = {
    parse(phrase(p), input, "test_source")
  }

}

class DocParserSpec extends AnyWordSpec with Matchers {

  "DocParser" should {
    "parse doc string" in new DocParserEnv {
      parse(docString, "this is doc") shouldBe DocString("this is doc")
    }

    "parse reference" in new DocParserEnv {
      parse(reference, "`pkg.ref`") shouldBe DocReference("pkg.ref", "pkg.ref")
    }

    "parse docs" in new DocParserEnv {
      parse(docs, "word 2 `ref` next. Second sentence. characters: (){}[]-=/") shouldBe Docs(
        Seq(
          DocString("word 2 "),
          DocReference("ref", "ref"),
          DocString(" next. Second sentence. characters: (){}[]-=/")
        ))
    }

  }

}
