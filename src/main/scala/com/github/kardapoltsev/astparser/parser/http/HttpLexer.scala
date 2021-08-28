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

import com.github.kardapoltsev.astparser.parser.BaseLexer

import scala.util.parsing.input.Positional

class HttpLexer extends BaseLexer {
  import HttpLexer._

  override type Token = HttpLexer.Token

  override def errorToken(msg: String): Token = {
    HttpLexer.Error(msg)
  }

  override def token: Parser[Token] = {
    cached | method | leftBrace | rightBrace | questionMark | ampersand | slash | lexeme
  }
  protected def lexeme: Parser[Lexeme] =
    opt('`') ~> rep1(lexemeChar) <~ opt('`') ^^ (x => Lexeme(x.mkString))

  //TODO: improve valid http elems
  private val specialCharacters = Set(
    '-',
    '_'
  )
  private def lexemeChar =
    elem("valid lexeme", x => x.isLetterOrDigit || specialCharacters(x))

  protected def method: Parser[Token] = {
    get | post | put | patch | delete
  }

  protected def get: Parser[Token] = acceptSeq("GET") ^^ { m =>
    Method(m.mkString)
  }
  protected def post: Parser[Token] = acceptSeq("POST") ^^ { m =>
    Method(m.mkString)
  }
  protected def put: Parser[Token] = acceptSeq("PUT") ^^ { m =>
    Method(m.mkString)
  }
  protected def patch: Parser[Token] = acceptSeq("PATCH") ^^ { m =>
    Method(m.mkString)
  }
  protected def delete: Parser[Token] = acceptSeq("DELETE") ^^ { m =>
    Method(m.mkString)
  }

  protected def cached: Parser[Token] = acceptSeq("CACHED") ^^ { _ =>
    CachedDirective()
  }

  protected def leftBrace    = '{' ^^^ LeftBrace()
  protected def rightBrace   = '}' ^^^ RightBrace()
  protected def questionMark = '?' ^^^ QuestionMark()
  protected def ampersand    = '&' ^^^ Ampersand()
  protected def slash        = '/' ^^^ Slash()

  override def whitespace: Parser[Any] = rep[Any] {
    elem("whitespace", _.isWhitespace)
  }

}

object HttpLexer {
  sealed trait Token                extends Positional
  case class LeftBrace()            extends Token
  case class RightBrace()           extends Token
  case class QuestionMark()         extends Token
  case class Ampersand()            extends Token
  case class Slash()                extends Token
  case class Lexeme(chars: String)  extends Token
  case class Error(msg: String)     extends Token
  case class Method(method: String) extends Token
  case class CachedDirective()      extends Token
}
