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

import org.scalatest.{Matchers, WordSpec}



class ParserSpec extends WordSpec with Matchers {
  "Parser" should {
    "parse schema info" in new ParserTestEnv {
      val in =
        """
          |schema api;
          |version 3;
        """.stripMargin
      val parsed = parse(schemaInfoExp, in)
      parsed._1.name shouldBe "api"
      parsed._2.get.value shouldBe 3
    }

    "parse reference" in new ParserTestEnv {
      parse(reference, "a") shouldBe Reference("a")
      parse(reference, "a.b.c") shouldBe Reference("a.b.c")
    }

    "parse import definition" in new ParserTestEnv {
      val in =
        """
          |import api.User;
        """.stripMargin
      val parsed = parse(importDefinition, in)
      parsed shouldBe Import("User", Reference("api.User"))
    }

    "parse type alias" in new ParserTestEnv {
      val in =
        """
          |type myAlias := api.User;
        """.stripMargin
      val parsed = parse(typeAlias, in)
      parsed shouldBe TypeAlias("myAlias", Reference("api.User"))
    }

    "parse external type definition" in new ParserTestEnv {
      val in =
        """
          |external type Int;
        """.stripMargin
      val parsed = parse(externalTypeDefinition, in)
      parsed shouldBe ExternalType("Int", Seq.empty)
    }

    "parse external type definition with type params" in new ParserTestEnv {
      val in =
        """
          |external type Vector[T];
        """.stripMargin
      val parsed = parse(externalTypeDefinition, in)
      parsed shouldBe ExternalType("Vector", Seq(TypeParameter("T", Seq.empty)))
    }

    "parse simple package definition" in new ParserTestEnv {
      val in =
        """
          |package a {
          |}
        """.stripMargin
      val parsed = parse(apackage, in)
      parsed shouldBe Package("a", Seq.empty)
    }

    "parse compact package definition" in new ParserTestEnv {
      val in =
        """
          |package a.b.c {
          |}
        """.stripMargin
      val parsed = parse(apackage, in)
      parsed shouldBe Package("a", Seq(Package("b", Seq(Package("c", Seq.empty)))))
    }

    "parse full type definition" in new ParserTestEnv {
      val in =
        s"""
           |type A {
           |  a
           |    param1: Int
           |  ;
           |}
         """.stripMargin
      val parsed = parse(typeDefinition, in)
      val expected = Type(
        name =  "a",
        typeArguments = Seq.empty,
        parents = Seq.empty,
        constructors = Seq(
          TypeConstructor(
            name = "a",
            maybeId = None,
            typeArguments = Seq.empty,
            arguments = Seq(
              Argument(
                name = "param1",
                `type` = TypeStatement(Reference("Int"), Seq.empty),
                docs = Seq.empty
              )
            ),
            docs = Seq.empty
          )
        ),
        docs = Seq.empty
      )
    }

    "parse full type definition with specific id" in new ParserTestEnv {
      val in =
        s"""
           |type A {
           |  a #000001
           |    param1: Int
           |  ;
           |}
         """.stripMargin
      val parsed = parse(typeDefinition, in)
      val expected = Type(
        name =  "a",
        typeArguments = Seq.empty,
        parents = Seq.empty,
        constructors = Seq(
          TypeConstructor(
            name = "a",
            maybeId = None,
            typeArguments = Seq.empty,
            arguments = Seq(
              Argument(
                name = "param1",
                `type` = TypeStatement(Reference("Int"), Seq.empty),
                docs = Seq.empty
              )
            ),
            docs = Seq.empty
          )
        ),
        docs = Seq.empty
      )
    }

    "parse call definition" in new ParserTestEnv {
      val docString = "Documentation for myCall"
      val in =
        s"""
          |/**$docString*/
          |call myCall : ParentA
          |  param1: Int
          |  param2: User
          |  = Void;
        """.stripMargin
      val parsed = parse(callDefinition, in)
      val expected = Call(
        name = "myCall",
        maybeId = None,
        arguments = Seq(
          Argument("param1", TypeStatement(Reference("Int"), Seq.empty), Seq.empty),
          Argument("param2", TypeStatement(Reference("User"), Seq.empty), Seq.empty)
        ),
        returnType = TypeStatement(Reference("Void"), Seq.empty),
        parents = Seq(Reference("ParentA")),
        docs = Seq(Documentation(docString))
      )
      parsed shouldBe expected
    }

    "save positions for all elements" in new ParserTestEnv {
      val src = """
        |schema api;
        |version 1;
        |external type Int;
        |type UserId := Int;
        |
        |package outer.inner {
        |  import p.B;
        |  type A {
        |    a
        |      param1: B
        |    ;
        |  }
        |}
        |
        |package p {
        |  type B {
        |    b;
        |  }
        |}
      """.stripMargin
      val parsed = parse(src, "source_name")

      parsed.pos shouldBe a[SourcePosition]

      def checkPosition(e: Element): Unit = {
        //println(s"checking ${e.humanReadable}")
        e.pos shouldBe a[SourcePosition]
        e.children foreach checkPosition
      }

      parsed.deepDefinitions foreach { d =>
        checkPosition(d)
      }
    }
  }

}
