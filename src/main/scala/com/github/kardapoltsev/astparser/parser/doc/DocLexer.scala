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

import com.github.kardapoltsev.astparser.parser.BaseLexer

import scala.util.parsing.input.Positional

class DocLexer extends BaseLexer {
  import DocLexer._
  override type Token = DocLexer.Token

  override def errorToken(msg: String): Token = {
    DocLexer.Error(msg)
  }

  override def token: Parser[Token] = {
    backtick | dot | identifier | specialCharacters | space
  }

  override def whitespace: Parser[Any] = rep[Any] {
    elem("whitespace", _ => false)
  }

  protected def backtick: Parser[Token] = '`' ^^^ BackTick()
  protected def dot                     = '.' ^^^ Dot()

  protected def identifier: Parser[Token] = {
    rep1(identifierChar) ^^ (x => Identifier(x.mkString))
  }
  private def identifierChar = elem("valid lexeme", x => x.isLetterOrDigit)

  protected def specialCharacters: Parser[Token] =
    rep1(specialChar) ^^ (x => SpecialCharacters(x.mkString))
  private val allowedCharacters = Set(
    '-', '+', '*', '=', ',', '_', ':', '~', '\'', '"', '(', ')', '[', ']', '{', '}', '<', '>', '/',
    '|', '\\', '!', '?'
  )
  private def specialChar =
    elem("special character", x => x.isLetterOrDigit || allowedCharacters(x))

  private def space = elem("space", x => x.isWhitespace) ^^^ Space()
}

object DocLexer {
  sealed trait Token                          extends Positional
  case class BackTick()                       extends Token
  case class Dot()                            extends Token
  case class Space()                          extends Token
  case class Identifier(chars: String)        extends Token
  case class SpecialCharacters(chars: String) extends Token
  case class Error(msg: String)               extends Token
}
