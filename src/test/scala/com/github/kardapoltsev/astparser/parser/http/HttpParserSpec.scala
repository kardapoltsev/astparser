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

import com.github.kardapoltsev.astparser.parser.ParseException
import org.scalatest.{Matchers, WordSpec}


trait HttpParserEnv extends HttpParser {

  protected def parse[T](p: Parser[T], input: String): T = {
    parse(phrase(p), input, "test_source")
  }

}

class HttpParserSpec extends WordSpec with Matchers {
  "HttpParser" should {

    "parse method" in new HttpParserEnv {
      parse(method, "GET") shouldBe HttpMethod("GET")
      parse(method, "POST") shouldBe HttpMethod("POST")
      parse(method, "PATCH") shouldBe HttpMethod("PATCH")
      parse(method, "DELETE") shouldBe HttpMethod("DELETE")
    }

    "not parse invalid method" in new HttpParserEnv {
      a[ParseException] shouldBe thrownBy {
        parse(method, "GE")
      }
    }

    "parse path segments" in new HttpParserEnv {
      parse(path, "/user/") shouldBe Seq(PathSegment("user"))
      parse(path, "/user") shouldBe Seq(PathSegment("user"))
      parse(path, "/users/me") shouldBe Seq(PathSegment("users"), PathSegment("me"))
    }

    "parse path parameters" in new HttpParserEnv {
      parse(path, "/users/{userId}") shouldBe Seq(PathSegment("users"), PathParam("userId"))
      parse(path, "/{param1}/{param2}/") shouldBe Seq(PathParam("param1"), PathParam("param2"))
    }

    "not parse invalid path" in new HttpParserEnv {
      a[ParseException] shouldBe thrownBy {
        parse(path, "/users/{userId}bug")
      }
      a[ParseException] shouldBe thrownBy {
        parse(path, "/users//")
      }
    }

    "parse http request" in new HttpParserEnv {
      override protected val enableProfiling: Boolean = true
      parse(request, "GET /api/users/{userId}?{param1}&{param2}") shouldBe
        HttpRequest(
          HttpMethod("GET"),
          Url(
            Seq(PathSegment("api"), PathSegment("users"), PathParam("userId")),
            Seq(QueryParam("param1"), QueryParam("param2"))
          )
        )
    }

  }
}
