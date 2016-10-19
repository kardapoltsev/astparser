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
      val maybeInner = m.getDefinition("api.v2.outer.inner")
      maybeInner shouldBe defined
      val inner = maybeInner.get
      inner shouldBe a[Package]
      val maybeTypeC = inner.asInstanceOf[Package].definitions.find(_.name == "C")
      maybeTypeC shouldBe defined
      val typeC = maybeTypeC.get

      val constructorC = m.getDefinition("api.v2.outer.inner.C.c").get
      val constructorCParent = m.getDefinition(constructorC.parent).get
      constructorCParent shouldBe a[Type]
      constructorCParent shouldBe typeC
    }
  }


  "accept traits as parents for type constructors" in {
    val model = buildModel(
      """
        |schema api
        |trait MyTrait
        |
        |type A {
        |  consA <: MyTrait
        |}
      """.stripMargin
    )
    val myTrait = model.getDefinition("api.v1.MyTrait").get
    val maybeA = model.getDefinition("api.v1.A")
    maybeA shouldBe defined
    val constructorA = maybeA.get.asInstanceOf[Type].constructors.head
    constructorA shouldBe a[TypeConstructor]
    constructorA.asInstanceOf[TypeConstructor].parents should contain(myTrait)
  }

  "accept constructors as an argument type" in {
    val model = buildModel(
      """
        |schema api
        |external type Int
        |
        |type A {
        |  a ::
        |    param: Int
        |  b ::
        |    param : a
        |}
      """.stripMargin
    )
    val maybeB = model.getDefinition("api.v1.A.b")
    maybeB shouldBe defined
    maybeB.get shouldBe a[TypeConstructor]
  }

}
