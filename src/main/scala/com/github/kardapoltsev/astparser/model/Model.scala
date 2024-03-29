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

import com.github.kardapoltsev.astparser.parser
import com.github.kardapoltsev.astparser.parser.AstParser
import com.github.kardapoltsev.astparser.parser.doc
import com.github.kardapoltsev.astparser.parser.doc.DocParser
import com.github.kardapoltsev.astparser.parser.http.HttpParser
import com.github.kardapoltsev.astparser.parser.http.HttpRequest
import com.github.kardapoltsev.astparser.parser.http.PathParam
import com.github.kardapoltsev.astparser.util.Logger

import scala.collection.immutable.ListMap

case class Model(
  schemas: Seq[Schema],
  private[astparser] val parsedModel: parser.Model
) extends Logger {

  private val definitions = deepDefinitions
    .groupBy(_.fullName)
    .map { case (fullName, defs) =>
      fullName -> defs
    }
    .withDefaultValue(Seq.empty)

  def deepDefinitions: Seq[Definition] = {
    schemas ++ (schemas flatMap (_.deepDefinitions))
  }

  def getDefinition(fullName: String): Seq[Definition] = {
    definitions(fullName)
  }

  def getType(typeReference: TypeReference): TypeLike = {
    getDefinition(typeReference.fullName) match {
      case Seq(t: TypeLike) => t
      case found if found.isEmpty =>
        throw new ReferenceNotFoundException(s"could not resolve $typeReference")
      case x => throw new ModelException(s"found unexpected entity for $typeReference: $x")
    }
  }

  def slice(from: Int, to: Int): Model = {
    this.copy(schemas = schemas.map(_.slice(VersionsInterval(Some(from), Some(to)))))
  }

  def filterConstrained(enabledFeatures: Seq[String]): Model = {
    val filteredSchemas = schemas.flatMap(_.filterConstrained(enabledFeatures))
    this.copy(schemas = filteredSchemas)
  }

  private[astparser] def validate(): Unit = loggingTime("validateModel") {
    val definitions = deepDefinitions
    log.info(s"validating model containing ${definitions.length} elements")

    def checkTypeReference(ref: TypeReference, parent: Definition): Unit = {
      try {
        this.getType(ref)
      } catch {
        case e: ModelException =>
          log.error(e)
          failValidation(s"could not resolve $ref from $parent")
      }
    }

    def checkArguments(args: Seq[Argument], parent: Definition): Unit = {
      args.foreach { a =>
        checkTypeReference(a.`type`.typeReference, parent)
      }
    }

    definitions.foreach {
      case d: Call =>
        checkTypeReference(d.returnType.typeReference, d)
        checkArguments(d.arguments, d)
      case d: Trait =>
        checkArguments(d.arguments, d)
      case d: TypeAlias =>
        checkTypeReference(d.`type`.typeReference, d)
      case d: TypeConstructor =>
        d.versions.foreach { cv =>
          checkArguments(cv.arguments, cv)
        }
      case _: TypeConstructorVersion                          => // nothing to do. It must be validated before
      case _: Package | _: ExternalType | _: Schema | _: Type => // nothing to check
    }
  }

  private def failValidation(msg: String): Unit = {
    log.error(msg)
    throw new ModelValidationException(msg)
  }

  validate()
}

object Model {
  private val astParser  = new AstParser()
  private val httpParser = new HttpParser()
  private val docParser  = new DocParser()

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
          definitions = convertDefinitions(s.definitions),
          constraint = convertConstraint(s.constraint)
        )
      },
      parsed
    )
  }

  private def convertConstraint(constraint: parser.Constraint): Constraint = {
    Constraint(
      EnableConstraint(constraint.enable.constraints),
      DisableConstraint(constraint.disable.constraints)
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
      convertDefinitions(p.definitions),
      convertConstraint(p.constraint)
    )
  }

  private def convertTrait(
    t: parser.Trait
  )(implicit m: parser.Model): Trait = {
    Trait(
      t.packageName,
      t.arguments map convertArgument,
      t.name,
      t.parents map resolve map (_.head) map convertParent,
      convertDocs(t.docs),
      convertConstraint(t.constraint)
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
      c.parents map resolve map (_.head) map convertParent,
      httpDefinition,
      convertVersionsInterval(c.versions),
      convertDocs(c.docs),
      convertConstraint(c.constraint)
    )
  }

  private def checkHttpParameters(call: parser.Call, http: HttpRequest): Unit = {
    val pathParams = http.url.path.collect { case PathParam(name) =>
      name
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
  )(ts: parser.TypeStatement)(implicit m: parser.Model): TypeStatement = {
    TypeStatement(
      ts.packageName,
      TypeReference(resolve(ts.ref).head.fullName),
      ts.typeArguments map convertTypeStatement(isTypeArgument = true),
      isTypeArgument
    )
  }

  private def convertExternalType(
    et: parser.ExternalType
  ): ExternalType = {
    ExternalType(
      et.packageName,
      et.fullName,
      et.typeArguments map convertTypeParameter,
      convertConstraint(et.constraint)
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
    // don't use groupBy to preserve constructors order
    var cm = ListMap.empty[String, List[parser.TypeConstructor]]
    t.constructors.foreach { c =>
      val before = cm.getOrElse(c.name, Nil)
      cm = cm.updated(c.name, before :+ c)
    }
    val constructors = cm.values.toSeq

    Type(
      t.packageName,
      name = t.name,
      typeArguments = t.typeArguments map convertTypeParameter,
      parents = t.parents map resolve map (_.head) map convertParent,
      constructors = constructors.map(convertTypeConstructor),
      docs = convertDocs(t.docs),
      convertConstraint(t.constraint)
    )
  }

  private def convertParent(p: parser.TypeLike)(implicit m: parser.Model): Parent = {
    p match {
      case t: parser.Trait        => convertTrait(t)
      case e: parser.ExternalType => convertExternalType(e)
      case other =>
        throw new Exception(s"expected trait or external type as parent, got ${other.humanReadable}")
    }
  }

  private def convertTypeAlias(
    a: parser.TypeAlias
  )(implicit m: parser.Model): TypeAlias = {
    TypeAlias(a.packageName, a.name, convertTypeStatement(a.`type`), convertConstraint(a.constraint))
  }

  private def resolve(ref: parser.Reference)(implicit m: parser.Model): Seq[parser.TypeLike] = {
    m.lookup(ref) match {
      case Seq(i: parser.Import) =>
        resolve(i.reference)
      case Seq(tl: parser.TypeLike) =>
        Seq(tl)
      case found if found.nonEmpty && found.forall(_.isInstanceOf[parser.TypeConstructor]) =>
        found.collect { case tc: parser.TypeConstructor => tc }
      case found if found.nonEmpty && found.forall(_.isInstanceOf[parser.Call]) =>
        found.collect { case c: parser.Call => c }
      case found if found.nonEmpty =>
        throw new Exception(s"${ref.humanReadable} resolved to unexpected $found")
      case _ =>
        throw new Exception(s"unresolved ${ref.humanReadable}")
    }
  }

  private def convertTypeConstructor(
    c: Seq[parser.TypeConstructor]
  )(implicit m: parser.Model): TypeConstructor = {
    assert(c.nonEmpty, "couldn't convert empty list of constructors")
    val name = c.head.name
    assert(c.forall(_.name == name), "constructors passed to convert should have the same name")
    val packageName = c.head.packageName
    val versions = c.map { constructor =>
      val parents = constructor.parents.map(resolve).map(_.head) map convertParent
      TypeConstructorVersion(
        parent = packageName,
        name = constructor.name,
        parents = parents,
        id = constructor.id,
        typeArguments = constructor.typeArguments map convertTypeParameter,
        arguments = constructor.arguments map convertArgument,
        versions = convertVersionsInterval(constructor.versions),
        docs = convertDocs(constructor.docs),
        constraint = convertConstraint(constructor.constraint)
      )
    }
    TypeConstructor(
      parent = packageName,
      name = name,
      versions = versions
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
          DocReference(r.name, TypeReference(resolve(ref).head.fullName))
      }
    }
    Documentation(elems)
  }

  private def convertVersionsInterval(
    v: parser.VersionsInterval
  ): VersionsInterval = {
    VersionsInterval(v.start, v.end)
  }

}
