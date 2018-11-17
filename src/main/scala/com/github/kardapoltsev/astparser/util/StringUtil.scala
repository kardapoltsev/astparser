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

package com.github.kardapoltsev.astparser.util

object StringUtil {
  implicit class StringOpt(private val self: String) extends AnyVal {
    def simpleName: String = {
      self.drop(self.lastIndexOf(".") + 1)
    }

    def packageName: String = {
      self.take(self.lastIndexOf("."))
    }

    def toPath: List[String] = {
      self.split("\\.").toList
    }

    //scalastyle:off method.name
    def ~(other: String): String = {
      val b = StringBuilder.newBuilder
      if (self != ".") {
        b.append(self)
      }
      if (b.nonEmpty && b.last != '.' && other.nonEmpty && other != ".") {
        b.append(".")
      }
      if (other.headOption.exists(_ == '.')) { //scala 2.10 don't have a contains
        b.append(other.tail)
      } else {
        b.append(other)
      }
      b.toString()
    }
    //scalastyle:on method.name
  }
}
