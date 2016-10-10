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

import java.io.{File, FileWriter}
import com.github.kardapoltsev.astparser.Hardcoded
import com.github.kardapoltsev.astparser.parser._
import com.github.kardapoltsev.astparser.model
import com.github.kardapoltsev.astparser.util.Logger



class SchemaGenerator(
  m: model.Model,
  filenameExtension: String
) extends Logger {
  import Hardcoded.{Keywords => K}
  private def ls = System.lineSeparator()

  def generate(): Seq[GeneratedFile] = loggingTime("formatting") {
    m.parsedModel.schemas flatMap { s =>
      s.versions map generateVersion
    }
  }

  private def generateVersion(version: SchemaVersion): GeneratedFile = {
    val filename = version.schema.name + ".v" + version.version + "." + filenameExtension
    val h = generateHeader(version)
    val c = (version.definitions map generateDefinition).mkString(ls)
    val content = h + c
    GeneratedFile(
      path = "",
      name = filename,
      content
    )
  }

  private def generateHeader(version: SchemaVersion): String = {
    s"""${K.Schema} ${version.schema.name}
       |${K.Version} ${version.version}
       |
       |""".stripMargin
  }

  private def generateDefinition(d: Definition): String = {
    d match {
      case p: PackageLike => generatePackage(p)
      case et: ExternalType => generateExternalType(et)
      case ta: TypeAlias => generateTypeAlias(ta)
      case t: Trait => generateTrait(t)
      case t: Type => generateType(t)
      case c: Call => generateCall(c)
      case i: Import => generateImport(i)
    }
  }

  private def generatePackage(p: PackageLike): String = {
    val c = p.definitions.map {
      case inner: PackageLike =>
        generatePackage(inner)
      case d: Definition => generateDefinition(d)
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
    s"${K.External} ${K.Type} ${et.fullName}${formatTypeParameters(et.typeArguments)}"
  }

  private def generateTypeAlias(ta: TypeAlias): String = {
    s"${K.Type} ${ta.name} = ${ta.reference.fullName}"
  }

  private def generateTrait(t: Trait): String = {
    val ext = formatParents(t.parents)
    s"${K.Trait} ${t.name}$ext".trim
  }

  private def generateType(t: Type): String = {
    val docs = formatDocs(t.docs)
    val c = t.constructors.map(generateConstructor).mkString(ls)
    val ext = formatParents(t.parents)
    s"""$docs
       |${K.Type} ${t.name}$ext {
       |${c.offset(2)}
       |}""".stripMargin
  }

  private def generateConstructor(c: TypeConstructor): String = {
    val docs = formatDocs(c.docs)
    val args = generateArguments(c.arguments)
    val id = formatId(c.maybeId)
    s"""$docs
       |${c.name}$id
       |${args.offset(2)}""".stripMargin
  }

  private def generateCall(c: Call): String = {
    val docs = formatDocs(c.docs)
    val ext = formatParents(c.parents)
    val id = formatId(c.maybeId)
    s"""$docs
       |${K.Call} ${c.name}$id$ext
       |${generateArguments(c.arguments).offset(2)}
       |  = ${formatTypeStatement(c.returnType)}""".stripMargin
  }

  private def generateArguments(args: Seq[Argument]): String = {
    if(args.nonEmpty) {
      val paramNameLength = args.map(n => escaped(n.name).length).max
      val typeLength = args.map(a => formatTypeStatement(a.`type`).length).max
      args.map { a =>
        val escapedName = escaped(a.name)
        val n = escapedName + " " * (paramNameLength - escapedName.length)
        val ft = formatTypeStatement(a.`type`)
        val ds = a.docs.map(_.content).mkString("").trim
        val d = if(ds.nonEmpty) s" -- $ds" else ""
        val t = if(ds.nonEmpty) ft + " " * (typeLength - ft.length) else ft
        s"$n : $t$d"
      }.mkString(ls)
    } else {
      ""
    }
  }

  private val keywords = Set(
    K.Import, K.External, K.Trait, K.Version, K.Call, K.Package,
    K.Schema, K.Type
  )

  private def escaped(v: String): String = {
    if(keywords(v)) {
      s"`$v`"
    } else {
      v
    }
  }

  private def formatDocs(docs: Seq[Documentation]): String = {
    if(docs.nonEmpty) {
      val lines = docs.flatMap(_.content.split(ls)).map(_.trim).filterNot(_.isEmpty).mkString(ls)
      s"""
         |/** $lines
         |  */""".stripMargin
    } else {
      ""
    }
  }

  private def formatParents(parents: Seq[Reference]): String = {
    if(parents.nonEmpty) {
      parents.map(_.fullName).mkString(" : ", " : ", "")
    } else {
      ""
    }
  }

  private def formatTypeStatement(ts: TypeStatement): String = {
    s"${ts.ref.fullName}${formatTypeArguments(ts.typeArguments.map(_.ref))}"
  }

  private def formatTypeParameters(args: Seq[TypeParameter]): String = {
    if(args.nonEmpty){
      args.map { a =>
        //a.ref.fullName + formatTypeArguments(a.typeArguments)
        a.name
      }.mkString("[", ",", "]")
    } else {
      ""
    }
  }
  private def formatTypeArguments(args: Seq[Reference]): String = {
    if(args.nonEmpty){
      args.map { a =>
        //a.ref.fullName + formatTypeArguments(a.typeArguments)
        a.fullName
      }.mkString("[", ",", "]")
    } else {
      ""
    }
  }

  private def formatId(maybeId: Option[Int]): String = {
    maybeId.map(id => f" #$id%02x").getOrElse("")
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
