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

import com.github.kardapoltsev.astparser.parser
import com.github.kardapoltsev.astparser.parser.AstParser
import com.github.kardapoltsev.astparser.parser.doc.{DocParser}
import com.github.kardapoltsev.astparser.parser.doc
import com.github.kardapoltsev.astparser.parser.http.{HttpParser, HttpRequest, PathParam}



case class Model(
  schemas: Seq[Schema],
  private[astparser] val parsedModel: parser.Model
) {

  private val definitions = deepDefinitions.groupBy(_.fullName).map {
    case (fullName, defs) =>
      fullName -> defs
  }.withDefaultValue(Seq.empty)

  def deepDefinitions: Seq[Definition] = {
    schemas ++ (schemas flatMap (_.deepDefinitions))
  }

  def getDefinition(fullName: String): Seq[Definition] = {
    definitions(fullName)
  }

  def getType(typeReference: TypeReference): TypeLike = {
    getDefinition(typeReference.fullName) match {
      case Seq(t: TypeLike) => t
      case x =>
        throw new Exception(s"found unexpected entity for $typeReference: $x")
    }
  }

  def slice(from: Int, to: Int): Model = {
    this.copy(schemas = schemas.map(_.slice(VersionsInterval(Some(from), Some(to)))))
  }

}


object Model {
  private val astParser = new AstParser()
  private val httpParser = new HttpParser()
  private val docParser = new DocParser()

  def build(schemas: Seq[java.io.File]): Model = {
    val parsed = schemas.map(f => astParser.parse(f))
    build(parser.Model(parsed))
  }
  //src -> src name
  def buildFromString(schemas: Seq[(String, String)]): Model = {
    val parsed = schemas.map { case (source, sourceName) => astParser.parse(source, sourceName) }
    build(parser.Model(parsed))
  }

  private def build(implicit parsed: parser.Model): Model = {
    Model(
      parsed.schemas map { s =>
        Schema(
          name = s.name,
          definitions = convertDefinitions(s.definitions)
        )
      },
      parsed
    )
  }

  private def convertDefinitions(
    definitions: Seq[parser.Definition]
  )(implicit m: parser.Model): Seq[Definition] = {
    definitions collect {
      case p: parser.Package =>
        convertPackage(p)
      case c: parser.Call =>
        convertCall(c)
      case t: parser.Type =>
        convertType(t)
      case ta: parser.TypeAlias =>
        convertTypeAlias(ta)
      case t: parser.Trait =>
        convertTrait(t)
      case et: parser.ExternalType =>
        convertExternalType(et)
    }
  }

  private def convertPackage(
    p: parser.Package
  )(implicit m: parser.Model): Package = {
    Package(
      p.packageName,
      p.name,
      convertDefinitions(p.definitions)
    )
  }

  private def convertTrait(
    t: parser.Trait
  )(implicit m: parser.Model): Trait = {
    Trait(
      t.packageName,
      t.arguments map convertArgument,
      t.name,
      t.parents map resolve map convertParent,
      convertDocs(t.docs)
    )
  }

  private def convertCall(
    c: parser.Call
  )(implicit m: parser.Model): Call = {
    val httpDefinition = c.httpRequest map { http =>
      httpParser.parse(
        http,
        s"http definition for ${c.humanReadable}"
      )
    }

    httpDefinition foreach { http =>
      checkHttpParameters(c, http)
    }

    Call(
      c.packageName,
      c.name,
      c.id,
      c.arguments map convertArgument,
      convertTypeStatement(c.returnType),
      c.parents map resolve map convertParent,
      httpDefinition,
      convertVersionsInterval(c.versions),
      convertDocs(c.docs)
    )
  }

  private def checkHttpParameters(call: parser.Call, http: HttpRequest): Unit = {
    val pathParams = http.url.path.collect {
      case PathParam(name) => name
    }
    val queryParams = http.url.query.map(_.name)
    pathParams ++ queryParams foreach { paramName =>
      call.arguments.find(_.name == paramName) match {
        case Some(_) =>
        case None =>
          throw new Exception(
            s"Parameter `$paramName` defined at ${http.pos} " +
              s"not found in `${call.name}` call definition"
          )
      }
    }
  }

  private def convertArgument(
    a: parser.Argument
  )(implicit m: parser.Model): Argument = {
    Argument(
      a.name,
      convertTypeStatement(a.`type`),
      convertDocs(a.docs)
    )
  }

  private def convertTypeStatement(
    ts: parser.TypeStatement
  )(implicit m: parser.Model): TypeStatement = {
    convertTypeStatement(isTypeArgument = false)(ts)
  }


  private def convertTypeStatement(
    isTypeArgument: Boolean
  )(
    ts: parser.TypeStatement
  )(implicit m: parser.Model): TypeStatement = {
    TypeStatement(
      ts.packageName,
      TypeReference(resolve(ts.ref).fullName),
      ts.typeArguments map convertTypeStatement(isTypeArgument = true),
      isTypeArgument
    )
  }

  private def convertTypeLike(
    tl: parser.TypeLike
  )(implicit m: parser.Model): TypeLike = {
    tl match {
      case t: parser.Type =>
        convertType(t)
      case c: parser.TypeConstructor =>
        convertTypeConstructor(c)
      case t: parser.Trait =>
        convertTrait(t)
      case ta: parser.TypeAlias =>
        convertTypeAlias(ta)
      case et: parser.ExternalType =>
        convertExternalType(et)
      case c: parser.Call =>
        convertCall(c)
    }
  }

  private def convertExternalType(
    et: parser.ExternalType
  )(implicit m: parser.Model): ExternalType = {
    ExternalType(
      et.packageName,
      et.fullName,
      et.typeArguments map convertTypeParameter
    )
  }

  private def convertTypeParameter(
    tp: parser.TypeParameter
  ): TypeParameter = {
    TypeParameter(
      tp.name,
      tp.typeParameters map convertTypeParameter
    )
  }

  private def convertType(t: parser.Type)(implicit m: parser.Model): Type = {
    Type(
      t.packageName,
      name = t.name,
      typeArguments = t.typeArguments map convertTypeParameter,
      parents = t.parents map resolve map convertParent,
      constructors = t.constructors map convertTypeConstructor,
      docs = convertDocs(t.docs)
    )
  }

  private def convertParent(p: parser.TypeLike)(implicit m: parser.Model): Parent = {
    p match {
      case t: parser.Trait => convertTrait(t)
      case e: parser.ExternalType => convertExternalType(e)
      case other =>
        throw new Exception(s"expected trait or external type as parent, got ${other.humanReadable}")
    }
  }

  private def convertTypeAlias(
    a: parser.TypeAlias
  )(implicit m: parser.Model): TypeAlias = {
    TypeAlias(a.packageName, a.name, convertTypeLike(resolve(a.reference)))
  }

  private def resolveTrait(ref: parser.Reference)(implicit m: parser.Model): parser.Trait = {
    resolve(ref) match {
      case t: parser.Trait =>
        t
      case x: parser.TypeLike =>
        throw new Exception(s"expected trait for ${ref.humanReadable}, got ${x.humanReadable}")
    }
  }

  private def resolve(ref: parser.Reference)(implicit m: parser.Model): parser.TypeLike = {
    m.lookup(ref) match {
      case Some(i: parser.Import) =>
        resolve(i.reference)
      case Some(tl: parser.TypeLike) =>
        tl
      case Some(x: parser.Definition) =>
        throw new Exception(s"${ref.humanReadable} resolved to unexpected ${x.humanReadable}")
      case None =>
        throw new Exception(s"unresolved ${ref.humanReadable}")
    }
  }

  private def convertTypeConstructor(
    c: parser.TypeConstructor
  )(implicit m: parser.Model): TypeConstructor = {
    TypeConstructor(
      c.packageName,
      name = c.name,
      id = c.id,
      typeArguments = c.typeArguments map convertTypeParameter,
      arguments = c.arguments map convertArgument,
      parents = c.parents map resolve map convertParent,
      convertVersionsInterval(c.versions),
      docs = convertDocs(c.docs)
    )
  }

  private def convertDocs(
    docs: Seq[parser.Documentation]
  )(implicit m: parser.Model): Documentation = {
    val elems = docs flatMap { d =>
      docParser.parse(d.content, d.pos.toString()).docs map {
        case plain: doc.DocString =>
          PlainDoc(plain.value)
        case r: doc.DocReference =>
          val ref = parser.Reference(r.reference)
          ref.setPos(r.pos)
          ref.maybeParent = Some(d)
          DocReference(r.name, TypeReference(resolve(ref).fullName))
      }
    }
    Documentation(elems)
  }

  private def convertVersionsInterval(
    v: parser.VersionsInterval
  )(implicit m: parser.Model): VersionsInterval = {
    VersionsInterval(v.start, v.end)
  }

}
