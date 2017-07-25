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
package com.github.kardapoltsev.astparser.parser.http

import org.scalatest.{Matchers, WordSpec}

class HttpLexerSpec extends WordSpec with Matchers {
  import HttpLexer._

  private val lexer = new HttpLexer
  private def scan(input: String): List[Token] = {
    lexer.scan(input)
  }

  "HttpLexer" should {
    "parse GET route" in {
      scan("GET /api/users/") shouldBe List(
        Method("GET"),
        Slash(), Lexeme("api"),
        Slash(), Lexeme("users"),
        Slash()
      )
    }

    "parse route params" in {
      scan("GET /api/{userId}/") shouldBe List(
        Method("GET"),
        Slash(), Lexeme("api"),
        Slash(), LeftBrace(), Lexeme("userId"), RightBrace(),
        Slash()
      )
    }

    "parse escaped route params" in {
      scan("GET /api/{`type`}/") shouldBe List(
        Method("GET"),
        Slash(), Lexeme("api"),
        Slash(), LeftBrace(), Lexeme("type"), RightBrace(),
        Slash()
      )
    }

    "parse query params" in {
      scan("GET /api?{param1}&{param2}") shouldBe List(
        Method("GET"),
        Slash(), Lexeme("api"),
        QuestionMark(),
        LeftBrace(), Lexeme("param1"), RightBrace(), Ampersand(),
        LeftBrace(), Lexeme("param2"), RightBrace()
      )
    }

    "parse escaped query params" in {
      scan("GET /api?{`call`}") shouldBe List(
        Method("GET"),
        Slash(), Lexeme("api"),
        QuestionMark(),
        LeftBrace(), Lexeme("call"), RightBrace()
      )
    }
  }
}
