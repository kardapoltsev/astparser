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

import java.io.{File, FileWriter}
import com.github.kardapoltsev.astparser.Hardcoded
import com.github.kardapoltsev.astparser.parser._
import com.github.kardapoltsev.astparser.model

class SchemaGenerator(
  m: model.Model,
  filenameExtension: String
) extends Generator {
  import Hardcoded.{Keywords => K}
  private def ls = System.lineSeparator()

  def generate(): Seq[GeneratedFile] = loggingTime("formatting") {
    m.parsedModel.schemas map generateSchema
  }

  private def generateSchema(schema: Schema): GeneratedFile = {
    val filename = schema.name + "." + filenameExtension
    val h        = generateHeader(schema)
    val c        = (schema.definitions flatMap generateDefinition).mkString(ls)
    val content  = h + c
    GeneratedFile(
      path = "",
      name = filename,
      content
    )
  }

  private def generateHeader(schema: Schema): String = {
    s"${K.Schema} ${schema.name}" + ls * 2
  }

  private def generateDefinition(d: Definition): Option[String] = {
    d match {
      case et: ExternalType   => Some(generateExternalType(et))
      case ta: TypeAlias      => Some(generateTypeAlias(ta))
      case t: Trait           => Some(generateTrait(t))
      case t: Type            => Some(generateType(t))
      case c: Call            => Some(generateCall(c))
      case i: Import          => Some(generateImport(i))
      case p: Package         => Some(generatePackage(p))
      case t: TypeConstructor => None //should be generated inside generateType
      case s: Schema          => None
      case m: Model           => None
    }
  }

  private def generatePackage(p: PackageLike): String = {
    val c = p.definitions.flatMap { d =>
      generateDefinition(d)
    }.mkString(System.lineSeparator())
    s"""
      |package ${p.name} {
      |${c.offset(2)}
      |}
      |""".stripMargin
  }

  private def generateImport(i: Import): String = {
    s"${K.Import} ${i.reference.fullName}"
  }

  private def generateExternalType(et: ExternalType): String = {
    val docs = formatDocs(et.docs)
    s"""$docs
       |${K.External} ${K.Type} ${et.fullName}${formatTypeParameters(et.typeArguments)}
       |""".stripMargin.trim
  }

  private def generateTypeAlias(ta: TypeAlias): String = {
    s"${K.Type} ${ta.name} = ${formatTypeStatement(ta.`type`)}"
  }

  private def generateTrait(t: Trait): String = {
    val docs = formatDocs(t.docs)
    val ext  = formatParents(t.parents)
    val args = generateArguments(t.arguments)
    s"""$docs
       |${K.Trait} ${t.name}$ext$args
       |""".stripMargin.trim
  }

  private def generateType(t: Type): String = {
    val docs        = formatDocs(t.docs)
    val c           = t.constructors.map(generateConstructor).mkString(ls)
    val ext         = formatParents(t.parents)
    val escapedName = escaped(t.name)
    s"""$docs
       |${K.Type} $escapedName$ext {
       |${c.offset(2)}
       |}""".stripMargin
  }

  private def generateConstructor(c: TypeConstructor): String = {
    val docs        = formatDocs(c.docs)
    val ext         = formatParents(c.parents)
    val args        = generateArguments(c.arguments)
    val id          = formatId(c.maybeId)
    val v           = formatVersion(c.versions)
    val escapedName = escaped(c.name)
    s"""$docs
       |$escapedName$id$v$ext$args""".stripMargin
  }

  private def generateCall(c: Call): String = {
    val docs = formatDocs(c.docs)
    val httpString = c.httpRequest match {
      case Some(http) => ls + "@" + http
      case None       => ""
    }
    val ext         = formatParents(c.parents)
    val id          = formatId(c.maybeId)
    val v           = formatVersion(c.versions)
    val escapedName = escaped(c.name)
    s"""$docs$httpString
       |${K.Call} $escapedName$id$v$ext${generateArguments(c.arguments)}
       |  => ${formatTypeStatement(c.returnType)}""".stripMargin
  }

  private def generateArguments(args: Seq[Argument]): String = {
    if (args.nonEmpty) {
      val paramNameLength = args.map(n => escaped(n.name).length).max
      val typeLength      = args.map(a => formatTypeStatement(a.`type`).length).max
      s" ::$ls" + args.map { a =>
        val escapedName = escaped(a.name)
        val n           = escapedName + " " * (paramNameLength - escapedName.length)
        val ft          = formatTypeStatement(a.`type`)
        val ds          = a.docs.map(_.content).mkString("").trim
        val d           = if (ds.nonEmpty) s" -- $ds" else ""
        val t           = if (ds.nonEmpty) ft + " " * (typeLength - ft.length) else ft
        s"$n : $t$d"
      }.mkString("", ls, "")
        .offset(2)
    } else {
      ""
    }
  }

  private val keywords = Set(
    K.Import,
    K.External,
    K.Trait,
    K.Call,
    K.Package,
    K.Schema,
    K.Type
  )

  private def escaped(v: String): String = {
    if (keywords(v)) {
      s"`$v`"
    } else {
      v
    }
  }

  private def formatDocs(docs: Seq[Documentation]): String = {
    if (docs.nonEmpty) {
      val lines = docs
        .flatMap(_.content.split(ls))
        .map(_.trim)
        .reverse
        .dropWhile(_.trim.isEmpty) //drop empty lines at the end
        .reverse
        .mkString(ls + " " * "/** ".length)
      s"""
         |/** $lines
         |  */""".stripMargin
    } else {
      ""
    }
  }

  private def formatParents(parents: Seq[Reference]): String = {
    if (parents.nonEmpty) {
      parents.map(_.fullName).mkString(" <: ", " ", "")
    } else {
      ""
    }
  }

  private def formatTypeStatement(ts: TypeStatement): String = {
    s"${ts.ref.fullName}${formatTypeArguments(ts.typeArguments)}"
  }

  private def formatTypeParameters(args: Seq[TypeParameter]): String = {
    if (args.nonEmpty) {
      args.map { a =>
        a.name + formatTypeParameters(a.typeParameters)
      }.mkString("[", ", ", "]")
    } else {
      ""
    }
  }
  private def formatTypeArguments(args: Seq[TypeStatement]): String = {
    if (args.nonEmpty) {
      args.map { a =>
        a.ref.fullName + formatTypeArguments(a.typeArguments)
      }.mkString("[", ", ", "]")
    } else {
      ""
    }
  }

  private def formatId(maybeId: Option[Int]): String = {
    maybeId.map(id => f" #$id%02x").getOrElse("")
  }

  private def formatVersion(version: VersionsInterval): String = {
    if (version.start.isEmpty && version.end.isEmpty) {
      ""
    } else {
      s" (${version.start.map(_.toString).getOrElse("")}-${version.end.map(_.toString).getOrElse("")})"
    }
  }

}

case class GeneratedFile(path: String, name: String, content: String) {
  def fullName: String = path + File.separator + name
}

object GeneratedFile {
  def write(generated: GeneratedFile, rootDirectory: String): File = {
    val parent = new File(rootDirectory, generated.path)
    if (!parent.exists())
      parent.mkdirs()
    val file = new File(parent, generated.name)
    if (file.exists())
      file.delete()
    file.createNewFile()
    val writer = new FileWriter(file)
    writer.write(generated.content)
    writer.close()
    file
  }
}
