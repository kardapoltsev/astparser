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

package com.github.kardapoltsev.astparser.parser.http

import com.github.kardapoltsev.astparser.parser.BaseParser
import com.github.kardapoltsev.astparser.parser.ReaderWithSourcePosition

import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.Positional

sealed trait PathElement extends Positional

case class QueryParam(name: String) {

  override def toString: String = {
    "{" + name + "}"
  }

}

case class PathSegment(segment: String) extends PathElement {

  override def toString: String = {
    segment
  }

}

case class PathParam(name: String) extends PathElement {

  override def toString: String = {
    "{" + name + "}"
  }

}

case class Url(path: Seq[PathElement], query: Seq[QueryParam]) extends Positional {

  override def toString: String = {
    val queryString = if (query.nonEmpty) query.mkString("?", "&", "") else ""
    path.mkString("/", "/", "") + queryString
  }

}

sealed trait HttpMethod extends Positional

case class Get() extends HttpMethod {
  override def toString: String = "GET"
}

case class Put() extends HttpMethod {
  override def toString: String = "PUT"
}

case class Post() extends HttpMethod {
  override def toString: String = "POST"
}

case class Delete() extends HttpMethod {
  override def toString: String = "DELETE"
}

case class Patch() extends HttpMethod {
  override def toString: String = "PATCH"
}

case class Cached() extends Positional

case class HttpRequest(method: HttpMethod, url: Url, cached: Boolean) extends Positional {

  override def toString: String = {
    method.toString + " " + url.toString
  }

}

class HttpParser(override protected val enableProfiling: Boolean = false) extends BaseParser {
  override type Elem = HttpLexer.Token
  import HttpLexer._

  private val lexer = new HttpLexer()

  def request: Parser[HttpRequest] = profile("request") {
    positioned {
      phrase {
        cached ~ method ~ url ^^ { case cached ~ m ~ url =>
          HttpRequest(m, url, cached = cached.isDefined)
        }
      }
    }
  }

  protected def cached: Parser[Option[Cached]] = {
    opt {
      accept(
        "CachedKeyword",
        { case _: CachedDirective =>
          Cached()
        },
      )
    }
  }

  protected def method: Parser[HttpMethod] =
    accept(
      "HttpMethod",
      {
        case Method("GET")    => Get()
        case Method("PUT")    => Put()
        case Method("POST")   => Post()
        case Method("PATCH")  => Patch()
        case Method("DELETE") => Delete()
      },
    )

  private def url: Parser[Url] = profile("url") {
    positioned {
      path ~ query ^^ { case pathSegments ~ querySegments =>
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
    identifier ^^ { s =>
      PathSegment(s)
    }
  }

  private def identifier: Parser[String] =
    accept(
      "pathSegment",
      { case Lexeme(chars) =>
        chars.toString
      },
    )

  private def query: Parser[Seq[QueryParam]] = profile("query") {
    opt(QuestionMark() ~> repsep(LeftBrace() ~> identifier <~ RightBrace(), Ampersand())) ^^ { querySegments =>
      querySegments.getOrElse(Seq.empty) map { s =>
        QueryParam(s)
      }
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
    parser: Parser[T],
    input: CharSequence,
    sourceName: String,
  ): ParseResult[T] = {
    val reader  = new ReaderWithSourcePosition(new CharSequenceReader(input), sourceName)
    val scanner = new lexer.Scanner(reader)
    parser(scanner)
  }

}
