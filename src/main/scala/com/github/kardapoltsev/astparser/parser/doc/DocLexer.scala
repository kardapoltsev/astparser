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
    backtick | dot | lexeme
  }

  override def whitespace: Parser[Any] = rep[Any] {
    elem("whitespace", _.isWhitespace)
  }

  protected def backtick: Parser[Token] = '`' ^^^ BackTick()
  protected def dot = '.' ^^^ Dot()
  protected def lexeme: Parser[Token] = rep1(lexemeChar) ^^ (x => Lexeme(x.mkString))
  private def lexemeChar = elem("valid lexeme", x => x.isLetter || x.isDigit)
}


object DocLexer {
  sealed trait Token extends Positional
  case class BackTick() extends Token
  case class Dot() extends Token
  case class Lexeme(chars: String) extends Token
  case class Error(msg: String) extends Token
}
