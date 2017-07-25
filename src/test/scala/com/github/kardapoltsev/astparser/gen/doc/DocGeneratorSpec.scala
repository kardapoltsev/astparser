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
package com.github.kardapoltsev.astparser.gen.doc

import com.github.kardapoltsev.astparser.TestBase

class DocGeneratorSpec extends TestBase {
  "DocGenerator" should {
    "calculate doc coverage" in {
      val model = buildModel(
        """
          |schema api
          |external type Int
          |external type Void
          |
          |type A {
          |  /** Doc for constructor a */
          |  a ::
          |  p1: Int -- doc for p1
          |
          |  anotherA ::
          |    p1: Int
          |    p2: Int -- doc for p2
          |}
          |
          |/** Doc for x */
          |call X ::
          | p1 : Int -- doc for p1
          | p2 : Int
          | => Void
          |
        """.stripMargin
      )
      val docGenerator = new DocGenerator {
        override def generate() = ???
      }
      val coverage = docGenerator.calculateDocsCoverage(model.schemas.head)

      coverage should contain(
        "types coverage: 0.0%"
      )
      coverage should contain(
        "constructors coverage: 50.0%"
      )
      coverage should contain(
        "constructors arguments coverage: 66.7%"
      )
      coverage should contain(
        "calls coverage: 100.0%"
      )
      coverage should contain(
        "calls params coverage: 50.0%"
      )
    }
  }
}
