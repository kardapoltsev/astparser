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

import com.github.kardapoltsev.astparser.TestBase

class ModelSpec extends TestBase {
  "Parser model" should {

    "resolve local references" in {
      val m = buildParserModel(
        """
          |schema api
          |
          |package outer.inner {
          |  type A {
          |    a ::
          |      param1: B
          |  }
          |
          |  type B {
          |    b
          |  }
          |}
        """.stripMargin)
      val maybeTypeA = m.findSchema("api").flatMap(_.getDefinition("outer.inner.A"))
      maybeTypeA shouldBe defined

      val typeA = maybeTypeA.get

      val typeBRef = typeA.asInstanceOf[Type].
        constructors.head.arguments.head.`type`.ref

      m.lookup(typeBRef) shouldBe defined
      m.lookup(typeBRef).get shouldBe a[Type]
      m.lookup(typeBRef).get.asInstanceOf[Type].name shouldBe "B"

    }

    "resolve local references #2" in {
      val m = buildParserModel(
        """
          |schema api
          |
          |package outer {
          |  package inner {
          |    type A {
          |      a ::
          |       param1: inner2.C
          |    }
          |  }
          |
          |  package inner2 {
          |    type C {
          |      c (1-2)
          |    }
          |  }
          |}
        """.stripMargin
      )
      val maybeTypeA = m.getDefinition("api.outer.inner.A")
      maybeTypeA shouldBe defined

      val typeA = maybeTypeA.get

      val typeBRef = typeA.asInstanceOf[Type].
        constructors.head.arguments.head.`type`.ref

      m.lookup(typeBRef) shouldBe defined
      m.lookup(typeBRef).get shouldBe a[Type]
      m.lookup(typeBRef).get.asInstanceOf[Type].fullName shouldBe "api.outer.inner2.C"

    }

    "resolve absolute references within one schema" in {
      val m = buildParserModel(
        """
          |schema api
          |
          |package p1 {
          |  type A {
          |    a ::
          |      param1: p2.B
          |
          |  }
          |}
          |package p2 {
          |  type B {
          |    b
          |  }
          |}
        """.stripMargin)
      val maybeTypeA = m.findSchema("api").flatMap(_.getDefinition("p1.A"))
      maybeTypeA shouldBe defined

      val typeA = maybeTypeA.get

      val typeBRef = typeA.asInstanceOf[Type].
        constructors.head.arguments.head.`type`.ref

      m.lookup(typeBRef) shouldBe defined
      m.lookup(typeBRef).get shouldBe a[Type]
      m.lookup(typeBRef).get.asInstanceOf[Type].name shouldBe "B"

    }

    "resolve absolute references from another schema" in {
      val s1 =
        """
          |schema s1
          |
          |package p1 {
          |  type A {
          |    a ::
          |      param1: s2.p2.C
          |
          |  }
          |}
        """.stripMargin
      val s2 =
        """
          |schema s2
          |
          |package p2 {
          |  type C {
          |    c
          |  }
          |}
        """.stripMargin
      val m = buildParserModel(s1, s2)
      val maybeTypeA = m.findSchema("s1").flatMap(_.getDefinition("p1.A"))
      maybeTypeA shouldBe defined

      val typeA = maybeTypeA.get

      val typeBRef = typeA.asInstanceOf[Type].
        constructors.head.arguments.head.`type`.ref

      m.lookup(typeBRef) shouldBe defined
      m.lookup(typeBRef).get shouldBe a[Type]
      m.lookup(typeBRef).get.asInstanceOf[Type].name shouldBe "C"
    }

    "resolve absolute reference to import from another schema" in {
      val s1 =
        """
          |schema s1
          |import s2.p2.C
          |
          |package p1 {
          |  type A {
          |    a ::
          |      param1: C
          |  }
          |}
        """.stripMargin
      val s2 =
        """
          |schema s2
          |package p2 {
          |  type C {
          |    c
          |  }
          |}
        """.stripMargin
      val m = buildParserModel(s1, s2)
      val maybeTypeA = m.findSchema("s1").flatMap(_.getDefinition("p1.A"))
      maybeTypeA shouldBe defined

      val typeA = maybeTypeA.get

      val argTypeRef = typeA.asInstanceOf[Type].
        constructors.head.arguments.head.`type`.ref

      m.lookup(argTypeRef) shouldBe defined
      m.lookup(argTypeRef).get shouldBe a[Import]
      m.lookup(argTypeRef).get.asInstanceOf[Import].name shouldBe "C"
    }

    "validate arguments in call" in {
      val s1 =
        """
          |schema api
          |external type Void
          |
          |package p1 {
          |  call a ::
          |      param1: p2.B
          |  => Void
          |}
        """.stripMargin
      an[ModelValidationException] shouldBe thrownBy {
        buildParserModel(s1)
      }
    }

    "handle imports" in {
      val m = buildParserModel(
        """
          |schema api
          |
          |package p1 {
          |  import p2.B
          |  type A {
          |    a ::
          |      param1: B
          |
          |  }
          |}
          |package p2 {
          |  type B {
          |    b
          |  }
          |}
        """.stripMargin)
      val api = m.findSchema("api").get
      val typeA = api.getDefinition("p1.A").get

      val typeBRef = typeA.asInstanceOf[Type].
        constructors.head.arguments.head.`type`.ref

      m.lookup(typeBRef) shouldBe defined
      m.lookup(typeBRef).get shouldBe a[Import]
      m.lookup(typeBRef).get.asInstanceOf[Import].name shouldBe "B"
      val i = m.lookup(typeBRef).get.asInstanceOf[Import]
      m.lookup(i.reference) shouldBe defined

    }

    "resolve references" in {
      val m = buildParserModel(
        """
          |schema common
          |external type Int
          |trait Object
          |trait Model <: Object
        """.stripMargin,
        """
          |schema api
          |import common.Int
          |type UserId = Int
          |trait Object <: common.Object
          |trait Model <: Object common.Model
          |
          |package p1 {
          |  type A <: Model {
          |    a ::
          |      param1: UserId
          |  }
          |  type B <: Model {
          |    b
          |  }
          |  package inner.inner2 {
          |    type C <: Model {
          |      c
          |    }
          |  }
          |}
        """.stripMargin
      )
      val api = m.findSchema("api").get
      val typeA = api.getDefinition("p1.A").get.asInstanceOf[Type]

      val typeBRef = typeA.constructors.head.arguments.head.`type`.ref

      m.lookup(typeBRef) shouldBe defined
      m.lookup(typeBRef).get shouldBe a[TypeAlias]
      m.lookup(typeBRef).get.asInstanceOf[TypeAlias].name shouldBe "UserId"
      val i = m.lookup(typeBRef).get.asInstanceOf[TypeAlias]
      m.lookup(i.reference) shouldBe defined

      val maybeObject = m.getDefinition("api.Object")
      maybeObject shouldBe defined
      val obj = maybeObject.get
      obj shouldBe a[Trait]
      obj.asInstanceOf[Trait].parents should have size 1
      val objParent = obj.asInstanceOf[Trait].parents.head
      objParent shouldBe a[Reference]
      val parentRef = objParent.asInstanceOf[Reference]
      m.lookup(parentRef) shouldBe defined
      m.lookup(parentRef).get shouldBe a[Trait]
      m.lookup(parentRef).get.asInstanceOf[Trait].fullName shouldBe "common.Object"

      val b = m.getDefinition("api.p1.B").get.asInstanceOf[Type]
      val bParent = m.lookup(b.parents.head)
      bParent shouldBe defined
      bParent.get.asInstanceOf[Trait].fullName shouldBe "api.Model"

      val model = m.getDefinition("api.Model").get
      model.asInstanceOf[Trait].parents should have size 2
      val objModelParent = model.asInstanceOf[Trait].parents.find(_.name == "Object").get
      m.lookup(objModelParent) shouldBe defined
      m.lookup(objModelParent).get.fullName shouldBe "api.Object"

      val typeC = m.getDefinition("api.p1.inner.inner2.C").get.asInstanceOf[Type]
      typeC.parents should have size 1
      m.lookup(typeC.parents.head).map(_.fullName) shouldBe Some("api.Model")
    }

    "resolve any valid references" in {
      noException shouldBe thrownBy {
        buildParserModel(
          """
            |schema common
            |external type Int
            |type A { a }
          """.stripMargin,
          """
            |schema api
            |import common.Int
            |type UserId = Int
            |
            |package p1 {
            |  trait C
            |  type A {
            |    a(2-) ::
            |      param1: UserId
            |  }
            |  type D <: C {
            |    d(1-1)
            |  }
            |}
          """.stripMargin
        )
      }
    }

    "validate duplicate ids" in {
      a[ModelValidationException] shouldBe thrownBy {
        buildParserModel(
          """
            |schema api
            |
            |external type Int
            |
            |package a {
            |  call b => Int
            |  call b => Int
            |}
            |
          """.stripMargin
        )
      }
    }

    "validate duplicate definitions" in {
      a[ModelValidationException] shouldBe thrownBy {
        buildParserModel(
          """
            |schema api
            |
            |external type Int
            |external type String
            |
            |package a {
            |  call b ::
            |   param1: Int
            |   param1: String
            |   => Int
            |}
            |
          """.stripMargin
        )
      }
    }

    "validate layer ranges" ignore {
      a[ModelValidationException] shouldBe thrownBy {
        buildParserModel(
          """
            |schema api
            |
            |external type Int
            |external type String
            |
            |package a {
            |  call b(1-2) ::
            |   param1: Int
            |   param2: String
            |   => Int
            |
            |  call b(2-2) ::
            |   param1: Int
            |   => Int
            |}
            |
          """.stripMargin
        )
      }
    }

    "validate trait fields" in {
      a[ModelValidationException] shouldBe thrownBy {
        buildParserModel(
          """
            |schema api
            |external type Int
            |
            |trait A ::
            |  arg1: Int
            |
            |type B <: A {
            |  b
            |}
            |
          """.stripMargin
        )
      }
    }

    "accept external types as parents" in {
      val model = buildParserModel(
        """
          |schema api
          |external type MyTrait
          |trait SchemaTrait
          |
          |type A <: MyTrait SchemaTrait {
          |  a
          |}
        """.stripMargin
      )
      val maybeA = model.getDefinition("api.A")
      maybeA shouldBe defined
      val typeA = maybeA.get
      typeA shouldBe a[Type]
      typeA.asInstanceOf[Type].parents should have size 2
    }

    "accept constructor as an argument type" in {
      val model = buildParserModel(
        """
          |schema api
          |external type Int
          |
          |type A {
          |  a ::
          |    param1: Int
          |  b ::
          |    param: a
          |}
        """.stripMargin
      )
      val maybeA = model.getDefinition("api.A")
      maybeA shouldBe defined
      val typeA = maybeA.get
      typeA shouldBe a[Type]
      typeA.asInstanceOf[Type].constructors should have size 2

      model.getDefinition("api.A.a") shouldBe defined
    }
  }

}
