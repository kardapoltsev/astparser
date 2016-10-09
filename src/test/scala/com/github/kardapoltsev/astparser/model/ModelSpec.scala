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
package com.github.kardapoltsev.astparser.model

import com.github.kardapoltsev.astparser.TestBase

class ModelSpec extends TestBase {

  "Model" should {
    "build" in {
      val s1 =
        """
          |schema api;
          |version 1;
          |
          |package outer.inner {
          |  type A {
          |    a
          |      param1: B
          |      `version`: Int
          |    ;
          |  }
          |
          |  type TypeAlias = A
          |
          |  type B : MyTrait {
          |    b;
          |  }
          |}
        """.stripMargin

      val s2 =
        """
          |schema api
          |version 2
          |
          |external type Int
          |external type MyTrait
          |
          |package outer.inner {
          |  type C {
          |    c
          |      param1: Int
          |    ;
          |  }
          |}
        """.stripMargin

      val m = buildModel(s1, s2)
      //println(m)
      m.schemas should have size 1
      m.schemas.head.latestVersion.version shouldBe 2
      m.deepDefinitions should have size 13
      val maybeInner = m.getDefinition("api.v2.outer.inner")
      maybeInner shouldBe defined
    }
  }

}
