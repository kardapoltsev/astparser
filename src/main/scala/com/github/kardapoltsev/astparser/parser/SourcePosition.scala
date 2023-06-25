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

import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.Position

case class SourcePosition(position: Position, sourceName: String) extends Position {
  override def column = position.column

  override def line = position.line

  override def lineContents: String = position match {
    case p: OffsetPosition => p.lineContents
    case p: SourcePosition => p.lineContents
    case NoPosition        => ""
    case x                 => throw new IllegalArgumentException(s"unknown position type ${x.getClass.getSimpleName}")
  }

  override def <(that: Position) = that match {
    case that: SourcePosition => this.position < that.position
    case that                 => this.position < that
  }

  override def toString: String = s"$sourceName($line, $column)"
}
