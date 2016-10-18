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
package com.github.kardapoltsev.astparser.gen

import com.github.kardapoltsev.astparser.TestBase



class SchemaGeneratorSpec extends TestBase {

  private def generate(source: String, sources: String*): Seq[GeneratedFile] = {
    val m =  buildModel(source, sources:_*)
    new SchemaGenerator(m, "sl").generate()
  }

  "SchemaGenerator" should {

    "generate header" in {
      val g = generate(
        """
          |schema api;
          |version 1;
        """.stripMargin
      )
      g should have size 1
      g.head.content shouldBe
        """schema api
          |version 1
          |
          |""".stripMargin
    }

    "generate external types" in {
      val g = generate(
        """
          |schema api;
          |version 1;
          |
          |external type Int;
        """.stripMargin)
      g.head.content shouldBe
        """schema api
          |version 1
          |
          |external type Int""".stripMargin
    }

    "generate imports" in {
      val g = generate(
        """
          |schema api;
          |version 1;
          |
          |package p1 {
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
          |version 1
          |
          |
          |package p1 {
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
          |version 1
          |
          |trait A
          |trait B <: A""".stripMargin

      generate(sample).head.content shouldBe sample
    }

    "generate type aliases" in {
      val g = generate(
        """
          |schema api;
          |version 1;
          |
          |external type Int
          |type UserId = Int
        """.stripMargin)
      g.head.content shouldBe
        """schema api
          |version 1
          |
          |external type Int
          |type UserId = Int""".stripMargin
    }

    "generate types" in {
      val g = generate(
        """
          |schema api
          |version 1
          |external type Int
          |external type String
          |trait T
          |
          |type MyType {
          |  myType ::
          |    param1: Int -- docs
          |    paramWithVeryLongName: String -- comment
          |}
          |
          |/** Docs for type B
          |    Very long
          |  */
          |type B <: T {
          |  b ::
          |    parameter: api.v1.MyType
          |    `version`   : Int
          |
          |  /** Docs for constructor c
          |    */
          |  c ::
          |    parameter: api.v1.MyType
          |}
        """.stripMargin)

      g.head.content shouldBe
        """schema api
          |version 1
          |
          |external type Int
          |external type String
          |trait T
          |
          |type MyType {
          |
          |  myType ::
          |    param1                : Int    -- docs
          |    paramWithVeryLongName : String -- comment
          |}
          |
          |/** Docs for type B
          |Very long
          |  */
          |type B <: T {
          |
          |  b ::
          |    parameter : api.v1.MyType
          |    `version` : Int
          |
          |  /** Docs for constructor c
          |    */
          |  c ::
          |    parameter : api.v1.MyType
          |}""".stripMargin
    }

    "generate calls" in {
      val g = generate(
        """
          |schema api;
          |version 1;
          |external type Int;
          |external type String;
          |external type Void;
          |call myCall ::
          |  param1: Int -- docs
          |  paramWithVeryLongName: String -- comment
          |  => Void;
        """.stripMargin)

      g.head.content shouldBe
        """schema api
          |version 1
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

    "generate packages" in {
      val g = generate("""
        |schema api;
        |version 1;
        |
        |package p1.p2 {
        |  package p3 {
        |
        |  }
        |}
      """.stripMargin)
      g.head.content shouldBe
        """schema api
          |version 1
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
          |schema api;
          |version 1;
          |external type Int
          |
          |call `call` ::
          |  param1 : Int
          |  => Int
        """.stripMargin
      )
      println(g.head.content)
      g.head.content shouldBe
        """schema api
          |version 1
          |
          |external type Int
          |
          |call `call` ::
          |  param1 : Int
          |  => Int""".stripMargin
    }
  }

}
