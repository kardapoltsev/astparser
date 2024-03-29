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

package com.github.kardapoltsev.astparser.gen

import com.github.kardapoltsev.astparser.TestBase

class SchemaGeneratorSpec extends TestBase {

  private def generate(sources: String*): Seq[GeneratedFile] = {
    val m         = buildModel(sources: _*)
    val generator = new SchemaGenerator(m, fileExt)
    val generated = generator.generate()

    //ensure that generated source is parsable
    generated.foreach { f =>
      noException shouldBe thrownBy {
        parser.parse(f.content, f.fullName)
      }
    }

    //ensure that generator is "stable"
    val formattedSchemas = generated.map(_.content)
    new SchemaGenerator(buildModel(formattedSchemas: _*), fileExt).generate() shouldBe generated

    generated
  }

  "SchemaGenerator" should {

    "generate header" in {
      val g = generate(
        """
          |schema api
        """.stripMargin
      )
      g should have size 1
      g.head.content shouldBe
        """schema api
          |
          |""".stripMargin
    }

    "generate external types" in {
      val g = generate("""
                         |schema api
                         |
                         |external type Int
        """.stripMargin)
      g.head.content shouldBe
        """schema api
          |
          |external type Int""".stripMargin
    }

    "generate imports" in {
      val g = generate("""
                         |schema api
                         |
                         |import p1.A
                         |import p2.B
                         |
                         |package p1 {
                         |  type A {
                         |    a
                         |  }
                         |  type B {
                         |    b
                         |  }
                         |}
                         |package p2 {
                         |  import p1.B
                         |}
        """.stripMargin)

      g.head.content shouldBe
        """schema api
          |
          |import p1.A
          |import p2.B
          |
          |package p1 {
          |
          |  type A {
          |
          |    a
          |  }
          |
          |  type B {
          |
          |    b
          |  }
          |}
          |
          |
          |package p2 {
          |  import p1.B
          |}
          |""".stripMargin
    }

    "generate traits" in {
      val sample =
        """schema api
          |
          |/** Doc for trait A
          |  */
          |trait A
          |trait B <: A
          |trait C ::
          |  arg1 : A""".stripMargin

      generate(sample).head.content shouldBe sample
    }

    "generate type aliases" in {
      val g = generate("""
                         |schema api
                         |
                         |external type Int
                         |type UserId = Int
        """.stripMargin)
      g.head.content shouldBe
        """schema api
          |
          |external type Int
          |type UserId = Int""".stripMargin
    }

    "generate types" in {
      val g = generate("""
                         |schema api
                         |external type Int
                         |external type String
                         |external type Vector
                         |external type Map
                         |trait T
                         |
                         |type MyType {
                         |  myType ::
                         |    param1: Int -- docs
                         |    paramWithVeryLongName: String -- comment
                         |    mapParam: Map[Int, Vector[Int]]
                         |}
                         |
                         |type MyEnum {
                         |  enumA <: T ::
                         |  enumB <: T ::
                         |}
                         |
                         |/** Docs for type B
                         |    Very long
                         |  */
                         |type B <: T {
                         |  b ::
                         |    parameter: api.MyType
                         |    `version`   : Int
                         |
                         |  /** Docs for constructor c
                         |    */
                         |  c ::
                         |    parameter: api.MyType
                         |}
        """.stripMargin)

      g.head.content shouldBe
        """schema api
          |
          |external type Int
          |external type String
          |external type Vector
          |external type Map
          |trait T
          |
          |type MyType {
          |
          |  myType ::
          |    param1                : Int                   -- docs
          |    paramWithVeryLongName : String                -- comment
          |    mapParam              : Map[Int, Vector[Int]]
          |}
          |
          |type MyEnum {
          |
          |  enumA <: T ::
          |
          |  enumB <: T ::
          |}
          |
          |/** Docs for type B
          |    Very long
          |  */
          |type B <: T {
          |
          |  b ::
          |    parameter : api.MyType
          |    version   : Int
          |
          |  /** Docs for constructor c
          |    */
          |  c ::
          |    parameter : api.MyType
          |}""".stripMargin
    }

    "generate calls" in {
      val g = generate("""
                         |schema api
                         |external type Int
                         |external type String
                         |external type Void
                         |call myCall ::
                         |  param1: Int -- docs
                         |  paramWithVeryLongName: String -- comment
                         |  => Void
        """.stripMargin)

      g.head.content shouldBe
        """schema api
          |
          |external type Int
          |external type String
          |external type Void
          |
          |call myCall ::
          |  param1                : Int    -- docs
          |  paramWithVeryLongName : String -- comment
          |  => Void""".stripMargin
    }

    "generate versioned calls" in {
      val g = generate("""
                         |schema api
                         |external type Int
                         |external type Void
                         |call myCall(-2) ::
                         |  param1: Int -- docs
                         |  => Void
        """.stripMargin)

      g.head.content shouldBe
        """schema api
          |
          |external type Int
          |external type Void
          |
          |call myCall (-2) ::
          |  param1 : Int -- docs
          |  => Void""".stripMargin
    }

    "generate calls with http definitions" in {
      val g = generate("""
                         |schema api
                         |external type Int
                         |external type Void
                         |@GET /api/call?{`type`}&{param2}
                         |call myCall ::
                         |  `type`: Int
                         |  param2: Int
                         |  => Void
        """.stripMargin)

      g.head.content shouldBe
        """schema api
          |
          |external type Int
          |external type Void
          |
          |@GET /api/call?{`type`}&{param2}
          |call myCall ::
          |  `type` : Int
          |  param2 : Int
          |  => Void""".stripMargin
    }

    "generate packages" in {
      val g = generate("""
                         |schema api
                         |
                         |package p1.p2 {
                         |  package p3 {
                         |
                         |  }
                         |}
      """.stripMargin)
      g.head.content shouldBe
        """schema api
          |
          |
          |package p1 {
          |
          |  package p2 {
          |
          |    package p3 {
          |
          |    }
          |  }
          |}
          |""".stripMargin
    }

    "escape call name" in {
      val g = generate(
        """
          |schema api
          |external type Int
          |
          |call `call` ::
          |  param1 : Int
          |  => Int
        """.stripMargin
      )
      g.head.content shouldBe
        """schema api
          |
          |external type Int
          |
          |call `call` ::
          |  param1 : Int
          |  => Int""".stripMargin
    }

    "generate constraints" in {
      val g = generate(
        """
          |? FeatureA
          |schema api
          |external type Int
          |
          |?! FeatureB
          |? FeatureC
          |call `call` ::
          |  param1 : Int
          |  => Int
        """.stripMargin
      )
      g.head.content shouldBe
        """? FeatureA
          |schema api
          |
          |external type Int
          |
          |? FeatureC
          |?! FeatureB
          |call `call` ::
          |  param1 : Int
          |  => Int""".stripMargin
    }
  }

}
