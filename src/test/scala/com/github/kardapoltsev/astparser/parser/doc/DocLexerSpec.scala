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

import org.scalatest.{Matchers, WordSpec}

class DocLexerSpec extends WordSpec with Matchers {
  import DocLexer._

  private val lexer = new DocLexer
  private def scan(input: String): List[Token] = {
    lexer.scan(input)
  }

  "DocLexer" should {
    "parse docs" in {
      scan("ref to `pkg.test`") shouldBe List(
        Identifier("ref"),
        Space(),
        Identifier("to"),
        Space(),
        BackTick(),
        Identifier("pkg"),
        Dot(),
        Identifier("test"),
        BackTick()
      )
    }
    "parse special characters" in {
      scan("word () wordWithCharacter[]") shouldBe List(
        Identifier("word"),
        Space(),
        SpecialCharacters("()"),
        Space(),
        Identifier("wordWithCharacter"),
        SpecialCharacters("[]")
      )
    }
  }

}
