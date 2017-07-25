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

import com.github.kardapoltsev.astparser.parser.{BaseParser, ReaderWithSourcePosition}

import scala.util.parsing.input.{CharSequenceReader, Positional}

sealed trait DocElement                                  extends Positional
case class DocString(value: String)                      extends DocElement
case class DocReference(name: String, reference: String) extends DocElement
case class Docs(docs: Seq[DocElement])                   extends Positional

class DocParser(override protected val enableProfiling: Boolean = false) extends BaseParser {
  override type Elem = DocLexer.Token
  import DocLexer._

  private val lexer = new DocLexer()

  def docs: Parser[Docs] = profile("request") {
    positioned {
      phrase {
        rep(Space()) ~> rep(docString | reference) <~ rep(Space()) ^^ { elems =>
          Docs(elems)
        }
      }
    }
  }

  protected def reference: Parser[DocReference] = profile("reference") {
    positioned {
      BackTick() ~> rep1sep(identifier, Dot()) <~ BackTick() ^^ { segments =>
        val name = segments.mkString(".")
        DocReference(name, name)
      }
    }
  }

  protected def docString: Parser[DocString] = profile("docs") {
    positioned {
      rep1(docWord) ^^ { words =>
        DocString(words.mkString(""))
      }
    }
  }

  private def docWord: Parser[String] =
    accept("doc word", {
      case Identifier(chars)        => chars
      case SpecialCharacters(chars) => chars
      case Dot()                    => "."
      case Space()                  => " "
    })

  private def identifier: Parser[String] =
    accept("identifier", {
      case Identifier(chars) => chars
    })

  def parse(input: CharSequence, sourceName: String): Docs = {
    loggingTime(s"parse $sourceName") {
      getResult(tryParse(docs, input, sourceName))
    }
  }

  protected def parse[T](parser: Parser[T], input: CharSequence, sourceName: String): T = {
    getResult(tryParse(parser, input, sourceName))
  }

  protected def tryParse[T](
    parser: Parser[T],
    input: CharSequence,
    sourceName: String
  ): ParseResult[T] = {
    val reader  = new ReaderWithSourcePosition(new CharSequenceReader(input), sourceName)
    val scanner = new lexer.Scanner(reader)
    parser(scanner)
  }

}
