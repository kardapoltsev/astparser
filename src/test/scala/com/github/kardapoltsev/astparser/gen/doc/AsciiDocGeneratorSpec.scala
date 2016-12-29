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
import com.github.kardapoltsev.astparser.gen.GeneratedFile

class AsciiDocGeneratorSpec extends TestBase {
  "AsciiDocGenerator" should {
    "generate docs" in {
      val model = buildModel(
        s"""
           |schema api
           |external type Int
           |external type Long
           |external type Void
           |
           |package outer.inner {
           |  /** Documentation for type A */
           |  type A {
           |    /**
           |     Multi line doc
           |     for constructor a
           |     */
           |    a ::
           |      param1: Int -- docs for type parameter
           |
           |    aEmpty
           |
           |    anotherA ::
           |      param1: Int
           |      param2: Long
           |  }
           |
           |  type B {
           |    b ::
           |      parameter: Int -- docs for parameter with link to `A`
           |  }
           |
           |  @GET /api/x/{pathParam}?{param}
           |  call X ::
           |    param: Long
           |    pathParam: Int
           |    => Void
           |}
           |
           |package p1 {
           |  /** Method returns type `outer.inner.A` */
           |  @POST /api/y
           |  call Y ::
           |    param: Int -- docs for call parameter
           |  => outer.inner.A
           |}
           |
         """.stripMargin)

      val generator = new AsciiDocGenerator(model, 1)

      val generated = generator.generate()
      generated should have size 1
      val apiDoc = generated.head.content

      apiDoc should include("= api")

      apiDoc should include(
       """|
          |[[A]]
          |=== A
          |Documentation for type A
          |""".stripMargin)
    }
  }
}
