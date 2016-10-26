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
import com.github.kardapoltsev.astparser.parser.http.{Get}



class ModelSpec extends TestBase {

  "Model" should {
    "build" in {
      val s1 =
        """
          |schema api
          |external type Int
          |external type MyTrait
          |
          |package outer.inner {
          |  type A {
          |    a ::
          |      param1: B
          |      `version`: Int
          |
          |  }
          |
          |  type TypeAlias = A
          |
          |  type B <: MyTrait {
          |    b
          |  }
          |
          |  type C {
          |    c(2-) ::
          |      param1: Int
          |
          |  }
          |}
        """.stripMargin

      val m = buildModel(s1)
      //println(m)
      m.schemas should have size 1
      val maybeInner = m.getDefinition("api.outer.inner")
      maybeInner shouldBe defined
      val inner = maybeInner.get
      inner shouldBe a[Package]
      val maybeTypeC = inner.asInstanceOf[Package].definitions.find(_.name == "C")
      maybeTypeC shouldBe defined
      val typeC = maybeTypeC.get

      val constructorC = m.getDefinition("api.outer.inner.C.c").get.asInstanceOf[TypeConstructor]
      constructorC.versions.contains(1) shouldBe false
      constructorC.versions.contains(2) shouldBe true

      val constructorCParent = m.getDefinition(constructorC.parent).get
      constructorCParent shouldBe a[Type]
      constructorCParent shouldBe typeC
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
    val myTrait = model.getDefinition("api.MyTrait").get
    val maybeA = model.getDefinition("api.A")
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
    val maybeB = model.getDefinition("api.A.b")
    maybeB shouldBe defined
    maybeB.get shouldBe a[TypeConstructor]
  }

  "handle http definitions" in {
    val model = buildModel(
      """
        |schema api
        |external type Int
        |external type User
        |
        |@GET /api/users/{userId}
        |call GetUser ::
        |  userId: Int
        |  => User
      """.stripMargin
    )
    val maybeGetUser = model.getDefinition("api.GetUser")
    maybeGetUser shouldBe defined
    maybeGetUser.get shouldBe a[Call]
    val getUser = maybeGetUser.get.asInstanceOf[Call]
    getUser.httpRequest shouldBe defined
    val http = getUser.httpRequest.get
    http.method shouldBe Get()
    http.url.path should have size 3
    http.url.query shouldBe empty
  }

  "check http parameters" in {
    an[Exception] shouldBe thrownBy {
      buildModel(
        """
          |schema api
          |external type Int
          |external type User
          |
          |@GET /api/users/{wrongParamName}
          |call GetUser ::
          |  userId: Int
          |  => User
        """.stripMargin
      )
    }
  }
  }

}
