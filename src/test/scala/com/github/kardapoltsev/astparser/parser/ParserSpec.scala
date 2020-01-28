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

package com.github.kardapoltsev.astparser.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserSpec extends AnyWordSpec with Matchers {
  "Parser" should {
    "parse schema info" in new ParserTestEnv {
      val in =
        """
          |schema api
        """.stripMargin
      val parsed = parse(schemaInfoExp, in)
      parsed.name shouldBe "api"
    }

    "print profiling info" in new ParserTestEnv {
      override val enableProfiling = true
      val in =
        """
          |schema api
        """.stripMargin
      parse(schemaInfoExp, in)
    }

    "parse reference" in new ParserTestEnv {
      parse(reference, "a") shouldBe Reference("a")
      parse(reference, "a.b.c") shouldBe Reference("a.b.c")
    }

    "versions interval" in new ParserTestEnv {
      parse(versionsInterval, "(2-3)") shouldBe VersionsInterval(Some(2), Some(3))
      parse(versionsInterval, "(-3)") shouldBe VersionsInterval(None, Some(3))
      parse(versionsInterval, "(2-)") shouldBe VersionsInterval(Some(2), None)
      parse(versionsInterval, "(-)") shouldBe VersionsInterval(None, None)
      parse(versionsInterval, "()") shouldBe VersionsInterval(None, None)
    }

    "parse import definition" in new ParserTestEnv {
      val in =
        """
          |import api.User
        """.stripMargin
      val parsed = parse(importDefinition, in)
      parsed shouldBe Import("User", Reference("api.User"))
    }

    "parse type alias syntax" in new ParserTestEnv {
      val in =
        """
          |type myAlias = api.User
        """.stripMargin
      val parsed = parse(typeAlias, in)
      parsed shouldBe TypeAlias("myAlias", TypeStatement(Reference("api.User")))
    }

    "parse complex type alias" in new ParserTestEnv {
      val in =
        """
          |type myAlias = Vector[api.User]
        """.stripMargin
      val parsed = parse(typeAlias, in)
      val t = TypeStatement(
        ref = Reference("Vector"),
        typeArguments = Seq(TypeStatement(Reference("api.User"))))
      parsed shouldBe TypeAlias("myAlias", t)
    }

    "parse external type definition" in new ParserTestEnv {
      val in =
        """
          |external type Int
        """.stripMargin
      val parsed = parse(externalTypeDefinition, in)
      parsed shouldBe ExternalType("Int", Seq.empty)
    }

    "parse external vector definition with type params" in new ParserTestEnv {
      val in =
        """
          |external type Vector[T]
        """.stripMargin
      val parsed = parse(externalTypeDefinition, in)
      parsed shouldBe ExternalType("Vector", Seq(TypeParameter("T", Seq.empty)))
    }

    "parse external map definition with type params" in new ParserTestEnv {
      val in =
        """
          |external type Map[K,V]
        """.stripMargin
      val parsed = parse(externalTypeDefinition, in)
      parsed shouldBe ExternalType(
        "Map",
        Seq(TypeParameter("K", Seq.empty), TypeParameter("V", Seq.empty)))
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
           |/** Docs for type A */
           |type TypeA {
           |  consA #000001 <: ParentA ParentB ::
           |    param1: Int
           |    param2: String
           |    param3: Map[String, Int]
           |}
         """.stripMargin
      val parsed = parse(typeDefinition, in)
      val expected = Type(
        name = "TypeA",
        typeArguments = Seq.empty,
        parents = Seq.empty,
        constructors = Seq(
          TypeConstructor(
            name = "consA",
            maybeId = Some(1),
            typeArguments = Seq.empty,
            arguments = Seq(
              Argument(
                name = "param1",
                `type` = TypeStatement(Reference("Int"), Seq.empty),
                docs = Seq.empty
              ),
              Argument(
                name = "param2",
                `type` = TypeStatement(Reference("String"), Seq.empty),
                docs = Seq.empty
              ),
              Argument(
                name = "param3",
                `type` = TypeStatement(
                  Reference("Map"),
                  Seq(
                    TypeStatement(Reference("String"), Seq.empty),
                    TypeStatement(Reference("Int"), Seq.empty))),
                docs = Seq.empty
              )
            ),
            parents = Seq(Reference("ParentA"), Reference("ParentB")),
            VersionsInterval(None, None),
            docs = Seq.empty
          )
        ),
        docs = Seq(Documentation(" Docs for type A "))
      )

      parsed shouldBe expected
    }

    "allow constructors with no parameters to extend trait" in new ParserTestEnv {
      val in =
        """
          |type MyClass {
          |  myClass <: MyTrait ::
          |
          |  anotherConstructor ::
          |    arg1: int
          |}
        """.stripMargin
      val parsed = parse(typeDefinition, in)
      parsed.constructors.head.parents shouldBe Seq(Reference("MyTrait"))
    }

    "parse trait definition" in new ParserTestEnv {
      val docString = "Documentation for MyTrait"
      val in =
        s"""
           |/**$docString*/
           |trait MyTrait <: ParentType ::
           |  arg1: Int
         """.stripMargin
      val parsed = parse(traitDefinition, in)
      val expected = Trait(
        name = "MyTrait",
        arguments = Seq(
          Argument("arg1", TypeStatement(Reference("Int"), Seq.empty), Seq.empty)
        ),
        parents = Seq(Reference("ParentType")),
        docs = Seq(Documentation(docString))
      )
      parsed shouldBe expected
    }

    "parse call definition" in new ParserTestEnv {
      val docString = "Documentation for myCall"
      val in =
        s"""
           |/**$docString*/
           |call myCall <: ParentA ParentB ::
           |  param1: Int
           |  param2: User
           |  => Void
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
        parents = Seq(Reference("ParentA"), Reference("ParentB")),
        httpRequest = None,
        VersionsInterval(None, None),
        docs = Seq(Documentation(docString))
      )
      parsed shouldBe expected
    }

    "parse call definition with http request" in new ParserTestEnv {
      val docString = "Documentation for myCall"
      val http      = "GET /api/myCall/{param1}"
      val in =
        s"""
           |/**$docString*/
           |@$http
           |call myCall <: ParentA ParentB ::
           |  param1: Int
           |  => Void
        """.stripMargin
      val parsed = parse(callDefinition, in)
      val expected = Call(
        name = "myCall",
        maybeId = None,
        arguments = Seq(
          Argument("param1", TypeStatement(Reference("Int"), Seq.empty), Seq.empty)
        ),
        returnType = TypeStatement(Reference("Void"), Seq.empty),
        parents = Seq(Reference("ParentA"), Reference("ParentB")),
        httpRequest = Some(http),
        VersionsInterval(None, None),
        docs = Seq(Documentation(docString))
      )
      parsed shouldBe expected
    }

    "save positions for all elements" in new ParserTestEnv {
      val src    = """
        |schema api
        |external type Int
        |type UserId = Int
        |
        |package outer.inner {
        |  import p.B
        |  type A {
        |    a ::
        |      param1: B
        |
        |  }
        |}
        |
        |package p {
        |  type B {
        |    b
        |  }
        |}
      """.stripMargin
      val parsed = parse(src, "source_name")

      parsed.pos shouldBe a[SourcePosition]
      parsed.pos.asInstanceOf[SourcePosition].lineContents should include("schema api")

      def checkPosition(e: Element): Unit = {
        e.pos shouldBe a[SourcePosition]
        e.children foreach checkPosition
      }

      parsed.deepDefinitions foreach { d =>
        checkPosition(d)
      }
    }

    "parse constraints" in new ParserTestEnv {
      val in =
        """
          |?A, B
          |?B
          |?C
          |?!D
        """.stripMargin
      val parsed = parse(constraint, in)
      parsed.enable.constraints should contain theSameElementsAs Seq("A", "B", "C")
    }

  }

}
