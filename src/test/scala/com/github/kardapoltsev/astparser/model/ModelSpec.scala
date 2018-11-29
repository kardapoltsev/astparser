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

package com.github.kardapoltsev.astparser.model

import com.github.kardapoltsev.astparser.TestBase
import com.github.kardapoltsev.astparser.parser.ModelValidationException
import com.github.kardapoltsev.astparser.parser.http.Get

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
      m.schemas should have size 1
      m.schemas.head.schemaName shouldBe "api"
      m.schemas.head.name shouldBe "api"
      val maybeInner = m.getDefinition("api.outer.inner").headOption
      maybeInner shouldBe defined
      val inner = maybeInner.get
      inner shouldBe a[Package]
      val maybeTypeC =
        inner.asInstanceOf[Package].definitions.find(_.name == "C")
      maybeTypeC shouldBe defined
      val typeC = maybeTypeC.get
      typeC.schemaName shouldBe "api"

      val constructorC = m
        .getDefinition("api.outer.inner.C.c")
        .headOption
        .get
        .asInstanceOf[TypeConstructor]
        .versions
        .head
      constructorC.versions.contains(1) shouldBe false
      constructorC.versions.contains(2) shouldBe true
      constructorC.schemaName shouldBe "api"
      constructorC.arguments.head.`type`.schemaName shouldBe "api"

      val constructorCParent =
        m.getDefinition(constructorC.parent).headOption.get
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
      val myTrait = model.getDefinition("api.MyTrait").head
      val maybeA  = model.getDefinition("api.A")
      maybeA should not be empty
      val constructorA = maybeA.head.asInstanceOf[Type].constructors.head
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
      maybeB should have size 1
      maybeB.head shouldBe a[TypeConstructor]
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
      maybeGetUser should not be empty
      maybeGetUser.head shouldBe a[Call]
      val getUser = maybeGetUser.head.asInstanceOf[Call]
      getUser.httpRequest shouldBe defined
      val http = getUser.httpRequest.get
      http.method shouldBe Get()
      http.url.path should have size 3
      http.url.query shouldBe empty
    }

    "support slices" in {
      val model = buildModel(
        """
          |schema api
          |external type Int
          |
          |call GetUser(1-2) ::
          |  userId: Int
          |  => User
          |
          |call GetUser(3-) ::
          |  userId: Int
          |  newParam: Int
          |  => User
          |
          |call GetUserNew(3-4) ::
          |  userId: Int
          |  => User
          |
          |call GetUserNewest(5-) ::
          |  userId: Int
          |  => User
          |
          |type User {
          |  user (1-1) ::
          |    id: Int
          |  user (2-5) ::
          |    id: Int
          |    param: Int
          |  userEmpty (5-10)
          |}
          |
          |call getUser(1-1) ::
          |  id: Int
          |  => User
          |
          |call getUser(2-) ::
          |  id: Int
          |  param: Int
          |  => User
          |
        """.stripMargin
      )
      model.getDefinition("api.GetUser") should have size 2
      model.getDefinition("api.GetUserNew") should have size 1
      model.getDefinition("api.GetUserNewest") should have size 1

      model.slice(2, 2).getDefinition("api.GetUser") should have size 1
      model
        .slice(2, 2)
        .getDefinition("api.GetUser")
        .head
        .asInstanceOf[Call]
        .arguments should have size 1

      model.slice(3, 5).getDefinition("api.GetUser") should have size 1
      model
        .slice(3, 5)
        .getDefinition("api.GetUser")
        .head
        .asInstanceOf[Call]
        .arguments should have size 2
      model.slice(3, 5).getDefinition("api.GetUserNew") should have size 1
      model.slice(3, 5).getDefinition("api.GetUserNewest") should have size 1

      model.slice(11, 11).getDefinition("api.User") shouldBe empty
      model
        .slice(2, 2)
        .getDefinition("api.User")
        .head
        .asInstanceOf[Type]
        .constructors should have size 1

      model.slice(3, 3).getDefinition("api.getUser") should have size 1
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

    "build documentation" in {
      val model = buildModel(
        """
          |schema api
          |external type Int
          |external type ExternalClass
          |
          |/**Docs for type A*/
          |type A {
          |  a ::
          |    param1: Int -- docs for param1 with a link to `ExternalClass`
          |}
        """.stripMargin
      )
      val maybeA = model.getDefinition("api.A").headOption
      maybeA shouldBe defined
      val typeA = maybeA.get
      typeA shouldBe a[Type]
      typeA.asInstanceOf[Type].docs shouldBe Documentation(
        Seq(
          PlainDoc("Docs for type A")
        ))
      val arg = typeA.asInstanceOf[Type].constructors.head.versions.head.arguments.head
      arg.docs.content should have size 2
      arg.docs.content.head shouldBe PlainDoc("docs for param1 with a link to ")
      arg.docs
        .content(1)
        .asInstanceOf[DocReference]
        .name shouldBe "ExternalClass"
      val ref = arg.docs.content(1).asInstanceOf[DocReference].reference
      model.getType(ref) shouldBe an[ExternalType]
    }

    "resolve references from imports" in {
      val m = buildModel("""
          |schema api
          |
          |package p1 {
          |  type A {
          |    a
          |  }
          |}
          |package p2 {
          |  import p1.A
          |  type B {
          |    b -- reference from import: `A.a`
          |  }
          |}
        """.stripMargin)

      val maybeTypeB = m.getDefinition("api.p2.B").headOption
      maybeTypeB shouldBe defined

      val typeB = maybeTypeB.get.asInstanceOf[Type]
      typeB.constructors.head.versions.head.docs.content.last shouldBe a[DocReference]
    }

    "handle inheritance" in {
      val m = buildModel("""
                           |schema api
                           |
                           |trait A
                           |trait B <: A
                           |trait C <: B
                           |trait D <: A
                           |
                         """.stripMargin)

      val traitA = m.getDefinition("api.A").head.asInstanceOf[Trait]
      val traitB = m.getDefinition("api.B").head.asInstanceOf[Trait]
      val traitC = m.getDefinition("api.C").head.asInstanceOf[Trait]
      val traitD = m.getDefinition("api.D").head.asInstanceOf[Trait]

      traitA.isSubtypeOf(traitA) shouldBe false
      traitB.isSubtypeOf(traitA) shouldBe true
      traitC.isSubtypeOf(traitA) shouldBe true
      traitD.isSubtypeOf(traitA) shouldBe true
      traitD.isSubtypeOf(traitB) shouldBe false
    }

    "group constructor versions" in {
      val m = buildModel("""
                           |schema api
                           |external type Int
                           |
                           |type A {
                           |  aImpl(1-1) ::
                           |    x: Int
                           |
                           |  aImpl(2-) ::
                           |    y: Int
                           |
                           |  aEmpty(1-)
                           |
                           |}
                           |
                         """.stripMargin)

      val typeA = m.getDefinition("api.A").head.asInstanceOf[Type]
      typeA.constructors should have size 2

      val constructorImpl = m.getDefinition("api.A.aImpl").head.asInstanceOf[TypeConstructor]
      constructorImpl.versions should have size 2

      val constructorEmpty = m.getDefinition("api.A.aEmpty").head.asInstanceOf[TypeConstructor]
      constructorEmpty.versions should have size 1
    }

    "not allow to extend self" in {
      a[ModelValidationException] shouldBe thrownBy {
        buildModel(
          """
            |schema api
            |
            |trait A <: A
            |""".stripMargin
        )
      }
    }
  }

}
