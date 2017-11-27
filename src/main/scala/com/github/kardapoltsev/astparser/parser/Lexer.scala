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

package com.github.kardapoltsev.astparser.parser

import scala.util.parsing.input._

sealed trait Token extends Positional

object Tokens {
  case class TypeKeyword()     extends Token
  case class PackageKeyword()  extends Token
  case class SchemaKeyword()   extends Token
  case class TraitKeyword()    extends Token
  case class CallKeyword()     extends Token
  case class ExternalKeyword() extends Token
  case class ImportKeyword()   extends Token

  case class Eq()    extends Token
  case class Colon() extends Token
  case class Dash()  extends Token
  case class Hash()  extends Token
  case class Dot()   extends Token
  case class Comma() extends Token

  case class LeftBrace()    extends Token
  case class RightBrace()   extends Token
  case class LeftBracket()  extends Token
  case class RightBracket() extends Token
  case class LeftParen()    extends Token
  case class RightParen()   extends Token

  case class LessSign()    extends Token
  case class GreaterSign() extends Token

  case class Lexeme(chars: String) extends Token

  case class Http(chars: String) extends Token

  trait Doc extends Token {
    def chars: String
  }

  case class LeftDoc(chars: String)  extends Doc
  case class RightDoc(chars: String) extends Doc

  case class Error(message: String) extends Token {
    def chars: String = s"ERROR: $message"
  }

  case object EOF extends Token {
    def chars: String = "<EOF>"
  }
}

class Lexer extends BaseLexer {
  override type Token = com.github.kardapoltsev.astparser.parser.Token
  import Tokens._
  import scala.util.parsing.input.CharArrayReader.EofCh

  override def whitespace: Parser[Any] = rep[Any](
    elem("", _.isWhitespace)
      | '/' ~ '/' ~ tillEndOfLine
      | '/' ~ '*' ~ not('*') ~ multilineCommentBody
  )

  protected def comment: Parser[Any] = (
    rep(noneOf(EofCh, '*')) ~ '*' ~ '/' ^^ { case _         => ' ' }
      | rep(noneOf(EofCh, '*')) ~ '*' ~ comment ^^ { case _ => ' ' }
  )

  protected def eq    = '=' ^^^ Eq()
  protected def colon = ':' ^^^ Colon()
  protected def dash  = '-' ^^^ Dash()
  protected def hash  = '#' ^^^ Hash()
  protected def dot   = '.' ^^^ Dot()
  protected def comma = ',' ^^^ Comma()

  protected def leftBrace  = '{' ^^^ LeftBrace()
  protected def rightBrace = '}' ^^^ RightBrace()

  protected def leftBracket  = '[' ^^^ LeftBracket()
  protected def rightBracket = ']' ^^^ RightBracket()

  protected def leftParen  = '(' ^^^ LeftParen()
  protected def rightParen = ')' ^^^ RightParen()

  protected def lessSign    = '<' ^^^ LessSign()
  protected def greaterSign = '>' ^^^ GreaterSign()

  import com.github.kardapoltsev.astparser.Hardcoded.{Keywords => K}
  private def typeKeyword: Parser[Token]    = keyword(K.Type, TypeKeyword())
  private def packageKeyword: Parser[Token] = keyword(K.Package, PackageKeyword())
  protected def schemaKeyword               = keyword(K.Schema, SchemaKeyword())
  protected def traitKeyword                = keyword(K.Trait, TraitKeyword())
  protected def callKeyword                 = keyword(K.Call, CallKeyword())
  protected def externalKeyword             = keyword(K.External, ExternalKeyword())
  protected def importKeyword               = keyword(K.Import, ImportKeyword())

  protected def restString = {
    (elem('@') ~> tillEndOfLine) ^^ { rest =>
      Http(rest)
    }
  }

  private def keyword(keyword: String, keywordToken: => Token): Parser[Token] = {
    acceptSeq(keyword) ~ ' ' ^^^ keywordToken
  }

  override def errorToken(msg: String) = Tokens.Error(msg)

  override def token: Parser[Token] = positioned(
    packageKeyword
      | typeKeyword
      | schemaKeyword
      | traitKeyword
      | restString
      | callKeyword
      | externalKeyword
      | importKeyword
      | lexeme
      | doc
      | symbol
      | eof
      | failure("illegal character")
  )

  private def eof = EofCh ^^^ EOF

  private def symbol: Parser[Token] =
    eq | colon | hash | dot | comma | leftBrace | rightBrace | leftBracket | rightBracket |
      leftParen | rightParen | dash | lessSign | greaterSign

  private def lexemeChar = elem("valid lexeme", x => x != EofCh && (x.isLetter || x.isDigit))

  private def lexeme: Parser[Lexeme] =
    opt('`') ~> rep1(lexemeChar) <~ opt('`') ^^ (x => Lexeme(x.mkString))

  private def lineDoc: Parser[RightDoc] = ('-' ~ '-') ~> tillEndOfLine ^^ RightDoc

  private def multilineDoc: Parser[LeftDoc] =
    ('/' ~ '*' ~ '*') ~> multilineCommentBody ^^ LeftDoc

  def doc: Parser[Doc] = lineDoc | multilineDoc

  private def tillEndOfLine = rep(noneOf(EofCh, '\n')) ^^ (_.mkString)

  private def multilineCommentBody: Parser[String] = {
    def seq = rep(noneOf('*', EofCh)) ^^ (_.mkString)

    (seq <~ ('*' ~ '/')
      | seq ~ ('*' ~> multilineCommentBody) ^^ {
        case a ~ b => a + "*" + b
      }
      | seq <~ eof ~ err("unclosed comment"))
  }

  private def noneOf(xs: Elem*): Parser[Elem] = elem("", e => !xs.contains(e))

}
