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

import org.scalatest.{Matchers, WordSpec}

class StringUtilSpec extends WordSpec with Matchers {
  import StringUtil._

  "StringUtil" should {

    "return simple name" in {
      "name".simpleName shouldBe "name"
      "package.name".simpleName shouldBe "name"
      "outer.package.name".simpleName shouldBe "name"
    }

    "return package name" in {
      "a".packageName shouldBe ""
      "a.b".packageName shouldBe "a"
      "a.b.c".packageName shouldBe "a.b"
    }

    "concatenate package names" in {
      "a" ~ "b" shouldBe "a.b"
      "a." ~ "b" shouldBe "a.b"
      "a" ~ ".b" shouldBe "a.b"
      "." ~ ".b" shouldBe "b"
      "a" ~ "." shouldBe "a"
    }

  }

}
