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

import com.github.kardapoltsev.astparser.parser.{ReaderWithSourcePosition, TokenParsers}

import scala.util.parsing.input.{CharSequenceReader, Positional}


sealed trait PathElement extends Positional
case class QueryParam(name: String)
case class PathSegment(segment: String) extends PathElement
case class PathParam(name: String) extends PathElement
case class Url(path: Seq[PathElement], query: Seq[QueryParam]) extends Positional
//case class HttpMethod(method: String) extends Positional
sealed trait HttpMethod extends Positional
case class Get() extends HttpMethod
case class Put() extends HttpMethod
case class Post() extends HttpMethod
case class Delete() extends HttpMethod
case class Patch() extends HttpMethod
case class HttpRequest(method: HttpMethod, url: Url) extends Positional



class HttpParser(override protected val enableProfiling: Boolean = false) extends TokenParsers {
  override type Elem = HttpLexer.Token
  import HttpLexer._

  private val lexer = new HttpLexer()

  def request: Parser[HttpRequest] = profile("request") {
    positioned {
      phrase {
        method ~ url ^^ { case m ~ url =>
          HttpRequest(m, url)
        }
      }
    }
  }


  protected def method: Parser[HttpMethod] = accept("HttpMethod", {
    case Method("GET") => Get()
    case Method("PUT") => Put()
    case Method("POST") => Post()
    case Method("PATCH") => Patch()
    case Method("DELETE") => Delete()
  })

  private def url: Parser[Url] = profile("url") {
    positioned {
      path ~ query ^^ {
        case pathSegments ~ querySegments =>
          Url(pathSegments, querySegments)
      }
    }
  }

  protected def path: Parser[Seq[PathElement]] = profile("path") {
    rep(Slash() ~> (pathSegment | pathParam)) <~ opt(Slash()) ^^ { segments =>
      segments
    }
  }

  private def pathParam: Parser[PathParam] = profile("pathParam") {
    LeftBrace() ~> identifier <~ RightBrace() ^^ { param =>
      PathParam(param)
    }
  }

  private def pathSegment: Parser[PathSegment] = {
    identifier ^^ { s => PathSegment(s)}
  }

  private def identifier: Parser[String] = accept("pathSegment", {
    case Lexeme(chars) => chars.toString
  })


  private def query: Parser[Seq[QueryParam]] = profile("query") {
    opt(QuestionMark() ~> repsep(LeftBrace() ~> identifier <~ RightBrace(), Ampersand())) ^^ {
      querySegments =>
        querySegments.getOrElse(Seq.empty) map { s => QueryParam(s) }
    }
  }


  private def tryParse(input: CharSequence, sourceName: String): ParseResult[HttpRequest] = {
    //val reader = new ReaderWithSourcePosition(new CharSequenceReader(input), sourceName)
    //val scanner = new lexer.Scanner(reader)
    //request(scanner)
    tryParse(request, input, sourceName)
  }

  def parse(input: CharSequence, sourceName: String): HttpRequest = {
    loggingTime(s"parse $sourceName") {
      getResult(tryParse(input, sourceName))
    }
  }

  protected def parse[T](parser: Parser[T], input: CharSequence, sourceName: String): T = {
    getResult(tryParse(parser, input, sourceName))
  }

  protected def tryParse[T](
    parser: Parser[T], input: CharSequence, sourceName: String
  ): ParseResult[T] = {
    val reader = new ReaderWithSourcePosition(new CharSequenceReader(input), sourceName)
    val scanner = new lexer.Scanner(reader)
    parser(scanner)
  }
}