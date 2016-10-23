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
import scala.util.parsing.input.{CharSequenceReader, Reader}

abstract class BaseLexer extends Scanners with Parsers {

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

}
