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
package com.github.kardapoltsev.astparser.parser

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.input._



sealed trait Token extends Positional

object Tokens {
  case class TypeKeyword() extends Token
  case class PackageKeyword() extends Token
  case class SchemaKeyword() extends Token
  case class TraitKeyword() extends Token
  case class VersionKeyword() extends Token
  case class CallKeyword() extends Token
  case class ExternalKeyword() extends Token
  case class ImportKeyword() extends Token

  case class Eq() extends Token
  case class Colon() extends Token
  case class Semicolon() extends Token
  case class Hash() extends Token
  case class Dot() extends Token

  case class LeftBrace() extends Token
  case class RightBrace() extends Token
  case class LeftBracket() extends Token
  case class RightBracket() extends Token

  case class LessSign() extends Token
  case class GreaterSign() extends Token

  case class Lexeme(chars: String) extends Token

  trait Doc extends Token {
    def chars: String
  }

  case class LeftDoc(chars: String) extends Doc
  case class RightDoc(chars: String) extends Doc

  case class Error(message: String) extends Token {
    def chars: String = s"ERROR: $message"
  }

  case object EOF extends Token {
    def chars: String = "<EOF>"
  }
}


class TokenReader(seq: Seq[Token]) extends Reader[Token] {
  override def atEnd = seq.isEmpty

  override def pos = {
    if(seq.nonEmpty) seq.head.pos
    else NoPosition
  }

  override def first = {
    if (seq.nonEmpty) seq.head
    else throw new RuntimeException("SeqReader at end")
  }

  override def rest =  {
    if (seq.nonEmpty) new TokenReader(seq.tail)
    else throw new RuntimeException("SeqReader at end")
  }
}


//noinspection ScalaStyle
class Lexer extends Scanners with Parsers {
  override type Token = com.github.kardapoltsev.astparser.parser.Token
  import Tokens._
  import scala.util.parsing.input.CharArrayReader.EofCh

  def scan(input: Reader[Char]): List[Token] = {
    var r = new Scanner(input)
    val buf = ListBuffer[Token]()
    while (!r.atEnd) {
      buf += r.first
      r = r.rest
    }
    buf.toList
  }

  def scan(input: CharSequence): List[Token] = scan(new CharSequenceReader(input))

  override def whitespace: Parser[Any] = rep[Any](
      elem("", _.isWhitespace)
    | '/' ~ '/' ~ tillEndOfLine
    | '/' ~ '*' ~ not('*') ~ multilineCommentBody
    )

  protected def comment: Parser[Any] = (
      rep (noneOf(EofCh, '*')) ~ '*' ~ '/'     ^^ { case _ => ' ' }
    | rep (noneOf(EofCh, '*')) ~ '*' ~ comment ^^ { case _ => ' ' }
    )

  protected def eq = '=' ^^ (_ => Eq())
  protected def colon = ':' ^^ (_ => Colon())
  protected def semicolon = ';' ^^ (_ => Semicolon())
  protected def hash = '#' ^^ (_ => Hash())
  protected def dot = '.' ^^ (_ => Dot())

  protected def leftBrace = '{' ^^ (_ => LeftBrace())
  protected def rightBrace = '}' ^^ (_ => RightBrace())

  protected def leftBracket = '[' ^^ (_ => LeftBracket())
  protected def rightBracket = ']' ^^ (_ => RightBracket())

  protected def lessSign = '<' ^^ (_ => LessSign())
  protected def greaterSign = '>' ^^ (_ => GreaterSign())

  import com.github.kardapoltsev.astparser.Hardcoded.{Keywords => K}
  private def typeKeyword: Parser[Token] = keyword(K.Type, TypeKeyword())
  private def packageKeyword: Parser[Token] = keyword(K.Package, PackageKeyword())
  protected def schemaKeyword = keyword(K.Schema, SchemaKeyword())
  protected def versionKeyword = keyword(K.Version, VersionKeyword())
  protected def traitKeyword = keyword(K.Trait, TraitKeyword())
  protected def callKeyword = keyword(K.Call, CallKeyword())
  protected def externalKeyword = keyword(K.External, ExternalKeyword())
  protected def importKeyword = keyword(K.Import, ImportKeyword())

  private def keyword(keyword: String, keywordToken: => Token): Parser[Token] = {
    acceptSeq(keyword) ~ ' ' ^^ (_ => keywordToken)
  }

  override def errorToken(msg: String) = Tokens.Error(msg)


  override def token: Parser[Token] = positioned(
      symbol
        | packageKeyword
        | typeKeyword
        | schemaKeyword
        | versionKeyword
        | traitKeyword
        | callKeyword
        | externalKeyword
        | importKeyword
        | lexeme
        | doc
        | eof
        | failure("illegal character")
  )

  private def eof = EofCh ^^^ EOF

  private def symbol: Parser[Token] =
        eq | colon | semicolon | hash | dot | leftBrace | rightBrace | leftBracket | rightBracket |
            lessSign | greaterSign

  private def lexemeChar = elem("valid lexeme", x => x != EofCh && (x.isLetter || x.isDigit))

  private def lexeme: Parser[Lexeme] = opt('`') ~> rep1(lexemeChar) <~ opt('`') ^^ (x => Lexeme(x.mkString))

  private def lineDoc: Parser[RightDoc] = ('-' ~ '-') ~> tillEndOfLine ^^ RightDoc

  private def multilineDoc: Parser[LeftDoc] =
    ('/' ~ '*' ~ '*') ~> multilineCommentBody ^^ LeftDoc

  def doc: Parser[Doc] = lineDoc | multilineDoc

  private def tillEndOfLine = rep(noneOf(EofCh, '\n')) ^^ (_.mkString)

  private def multilineCommentBody: Parser[String] = {
    def seq = rep(noneOf('*', EofCh)) ^^ (_.mkString)

    ( seq <~ ('*' ~ '/')
    | seq ~ ('*' ~> multilineCommentBody) ^^ {
        case a ~ b => a + "*" + b
      }
    | seq <~ eof ~ err("unclosed comment"))
  }

  private def multilineComment: Parser[Any] =
    ('/' ~ '*' ~ guard(not('*'))) ~> multilineCommentBody

  private def noneOf(xs: Elem*): Parser[Elem] = elem("", e => !xs.contains(e))

}
