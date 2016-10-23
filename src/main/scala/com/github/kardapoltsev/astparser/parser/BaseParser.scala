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

import com.github.kardapoltsev.astparser.util.Logger

import scala.util.parsing.combinator.Parsers


class BaseParser extends Parsers with Logger {

  protected def getResult[T](res: ParseResult[T]): T = res match {
    case Success(v, _) =>
      v
    case NoSuccess(msg, next) =>
      throw new ParseException(s"$msg; at `${next.first}`:${next.pos}", next.pos)
  }

}
