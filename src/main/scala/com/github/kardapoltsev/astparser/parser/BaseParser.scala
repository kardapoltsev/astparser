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

import com.github.kardapoltsev.astparser.parser.TokenParsers.Identifier
import com.github.kardapoltsev.astparser.util.Logger

import scala.util.parsing.combinator.Parsers

@SuppressWarnings(Array("scalafix:DisableSyntax.println"))
class BaseParser extends Parsers with Logger {

  protected def getResult[T](res: ParseResult[T]): T = res match {
    case Success(v, _) =>
      v
    case NoSuccess(msg, next) =>
      throw new ParseException(s"$msg; at `${next.first}`:${next.pos}", next.pos)
  }

  protected val enableProfiling: Boolean = false

  private class Printer(val prefix: String = "### ", val indent: Int = 0) {
    def println(str: String): Unit = {
      val p = prefix + " " * indent
      Predef.println(str.linesIterator.map(l => p + l).mkString(s"\n"))
    }

    def newIndent() = new Printer("", indent + 1)
  }

  private var printers: List[Printer] = List(new Printer())

  private def printer = printers.head

  private def println(str: String) = printer.println(str)

  private def printerIndent() = {
    printers = new Printer(printer.prefix, printer.indent + 1) :: printers
  }

  private def printerUnindent() = {
    printers = printers.tail
  }

  protected def profile[T](name: String)(p: Parser[T]) = {
    if (enableProfiling)
      Parser { in =>
        println(s"{ $name --- start parsing")
        printerIndent()
        val start = System.currentTimeMillis
        val res   = p(in)
        val end   = System.currentTimeMillis
        val t = res match {
          case Success(xs: Iterable[_], _) => s"${xs.map(_.getClass.getSimpleName)}"
          case Success(x: Definition, _)   => s"${x.getClass.getSimpleName}: name = ${x.name}"
          case Success(x: Argument, _)     => s"${x.getClass.getSimpleName}: $x"
          case Success(x: Identifier, _)   => s"${x.getClass.getSimpleName}: $x"
          case Success(x: Reference, _)    => s"${x.getClass.getSimpleName}: ${x.fullName}"
          case Success(x, _)               => x.getClass.getSimpleName
          case x @ NoSuccess(m, _)         => s"${x.getClass.getSimpleName}: $m"
        }
        printerUnindent()
        println(s"} $name => $t - parse in ${end - start}ms")
        res
      } else p
  }

}
