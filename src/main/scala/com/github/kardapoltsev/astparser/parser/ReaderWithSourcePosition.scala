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

import scala.util.parsing.input.Reader

final class ReaderWithSourcePosition[+T](reader: Reader[T], sourceName: String) extends Reader[T] {
  override def source: CharSequence = reader.source

  override def offset: Int = reader.offset

  override def atEnd = reader.atEnd

  override def pos = SourcePosition(reader.pos, sourceName)

  override def first = reader.first

  override def rest = new ReaderWithSourcePosition(reader.rest, sourceName)
}
