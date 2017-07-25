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

import com.github.kardapoltsev.astparser.gen.{GeneratedFile}
import com.github.kardapoltsev.astparser.model._

class AsciiDocGenerator(
  m: Model,
  targetVersion: Int
) extends DocGenerator {
  import AsciiDocGenerator._
  val model = m.slice(targetVersion, targetVersion)

  override def generate(): Seq[GeneratedFile] = {
    model.schemas flatMap generateSchema
  }

  private def generateSchema(schema: Schema): Seq[GeneratedFile] = {
    log.debug(s"generating docs for `${schema.fullName}`")
    val fullDoc = generateDefinition(schema).get
    Seq(GeneratedFile(".", schema.fullName + s"-v$targetVersion.ad", fullDoc.render))
  }

  private def generateDefinition(d: Definition): Option[DocNode] = {
    d match {
      case schema: Schema =>
        val innerDocs = schema.definitions flatMap generateDefinition
        val schemaDoc = DocumentInfo(schema.name, targetVersion)
        Some(Group(schemaDoc +: innerDocs))
      case p: Package =>
        val innerDocs = p.definitions flatMap generateDefinition
        val pd        = packageDoc(p)
        Some(Paragraph(pd +: innerDocs))
      case c: Call =>
        Some(methodDoc(c))
      case t: Type =>
        val td = typeDoc(t)
        val cd = t.constructors map { c =>
          constructorDoc(t, c)
        }
        Some(Group(td +: cd))
      case _ =>
        None
    }
  }

  private def packageDoc(p: Package): DocNode = {
    Header(p.fullName, p.name, 1)
  }

  private def methodDoc(m: Call): DocNode = {
    val docs =
      if (m.docs.content.nonEmpty)
        Seq(renderDocs(m.docs))
      else
        Seq.empty
    Group(
      Header(m.fullName, m.name, 2),
      Paragraph(
        Seq(
          Text("Result type: "),
          linkified(m.returnType),
          LineBreak
        ) ++ docs),
      httpString(m),
      Paragraph(paramsTable(m.arguments))
    )
  }

  private def typeDoc(t: Type): DocNode = {
    Paragraph(
      Header(t.fullName, t.name, 2),
      renderDocs(t.docs)
    )
  }

  private def constructorDoc(t: Type, c: TypeConstructor): DocNode = {
    Group(
      Seq(
        Header(c.fullName, c.name, 3),
        Paragraph(renderDocs(c.docs)),
        Paragraph(paramsTable(c.arguments))
      )
    )
  }

  private def linkified(definition: Definition): DocNode = {
    AnchorLink(definition.name, definition.fullName)
  }

  private def linkified(`type`: TypeStatement): DocNode = {
    val aType = model.getType(`type`.typeReference)
    if (`type`.typeArguments.nonEmpty) {
      val links = `type`.typeArguments map linkified
      Group(
        linkified(aType) +: Text("[") +: links :+ Text("]")
      )
    } else {
      linkified(aType)
    }
  }

  private def renderDocs(docs: Documentation): DocNode = {
    val nodes = docs.content.map {
      case PlainDoc(content) =>
        Text(content)
      case DocReference(name, reference) =>
        val aType = model.getType(reference)
        AnchorLink(name, aType.fullName)
    }
    Group(nodes)
  }

  private def paramsTable(params: Seq[Argument]): Table = {
    Table("", None, params.map { p =>
      Seq(
        Text(p.name),
        linkified(p.`type`),
        renderDocs(p.docs)
      )
    })
  }

  private def httpString(m: Call): DocNode = {
    m.httpRequest match {
      case Some(request) =>
        Paragraph(SourceCode(request.toString))
      case None =>
        Group(Seq.empty)
    }
  }

}

object AsciiDocGenerator {
  private[doc] sealed trait DocNode {
    def render: String
  }

  private[doc] case class Paragraph(content: Seq[DocNode]) extends DocNode {
    override def render: String = content.map(_.render).mkString("", "", "\n")
  }
  private[doc] object Paragraph {
    def apply(c1: DocNode, content: DocNode*): Paragraph = {
      Paragraph(c1 +: content)
    }
  }

  private[doc] case class Group(content: Seq[DocNode]) extends DocNode {
    override def render: String = content.map(_.render).mkString("")
  }
  private[doc] object Group {
    def apply(c1: DocNode, content: DocNode*): Group = {
      Group(c1 +: content)
    }
  }

  private[doc] case class Text(content: String) extends DocNode {
    override def render: String = content
  }

  private[doc] case class SourceCode(content: String) extends DocNode {
    override def render: String = {
      "\n\n[source,options=\"nowrap\"]\n" +
        "----\n" +
        content + "\n" +
        "----\n"
    }
  }

  private[doc] case class AnchorLink(content: String, anchor: String) extends DocNode {
    override def render: String = s"<<$anchor,$content>>"
  }

  private[doc] case class RelativeLink(url: String, text: String) extends DocNode {
    override def render: String = s"link:$url[$text]"
  }

  private[doc] case class DocumentInfo(title: String, version: Int) extends DocNode {
    override def render: String =
      s"""= $title
         |:version: $version
         |:toc2:
         |:toclevels: 4
         |:icons:
         |:max-width: 60em
         |""".stripMargin
  }

  private[doc] case class Header(anchor: String, content: String, level: Int) extends DocNode {
    override def render: String = {
      s"\n[[$anchor]]\n" + ("=" * (level + 1)) + " " + content + "\n"
    }
  }

  private[doc] case class UnorderedList(items: Seq[DocNode]) extends DocNode {
    override def render: String =
      if (items.isEmpty)
        ""
      else
        items.flatMap {
          case _: Table | LineBreak | _: Header | _: DocumentInfo =>
            throw new IllegalArgumentException()
          case block =>
            val lines = block.render.split("\n")
            if (lines.isEmpty)
              Seq.empty
            else
              Seq("- " + lines.head) ++ lines.tail.map("  " + _)
        }.mkString("", "\n", "")

  }

  private[doc] case class Table(name: String,
                                headers: Option[Seq[DocNode]],
                                rows: Seq[Seq[DocNode]])
      extends DocNode {
    val width = headers.map(_.size).getOrElse(0) max
      (if (rows.isEmpty) 0 else rows.maxBy(_.size).size)
    override def render: String = {
      val width = headers.map(_.size).getOrElse(0) max
        (if (rows.isEmpty) 0 else rows.maxBy(_.size).size)
      val lines = Seq(
        s"""[width="100%",cols="$width",frame="topbot",grid="rows"]""",
        s"""[options="autowidth${headers.fold("")(_ => ",header")}"]""",
        "|======"
      ) ++ (headers.map(h => h ++ Seq.fill(width - h.length)(" ")) ++ rows.map(_.map(_.render)))
        .map(_.mkString("|", "|", "")) :+
        "|======"
      lines.mkString("\n", "\n", "")
    }
  }

  private[doc] case object LineBreak extends DocNode {
    override def render: String = " +\n"
  }

  private[doc] case class Page(title: String, body: DocNode)

}
