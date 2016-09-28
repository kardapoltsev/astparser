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
import com.github.kardapoltsev.astparser.util.StringUtil._



trait AstElement {
  def parent: String
}

trait NamedElement extends AstElement {
  def packageName: String
  def name: String
  def fullName = packageName ~ name
}

trait Definition extends NamedElement {
  def parent: String
  def packageName = parent
}

trait TypeId {
  def id: Int
  def idHex: String = f"$id%02x"
  def idString: String = "TODO"
}

trait Documented {
  def docs: Seq[Documentation]
}

case class Documentation(
  content: String
)

case class Argument(
  name: String,
  `type`: TypeStatement,
  docs: Seq[Documentation]
) extends Documented

trait TypeLike extends Definition {
  def parents: Seq[Trait]
}

case class Type(
  parent: String,
  name: String,
  typeArguments: Seq[TypeParameter],
  parents: Seq[Trait],
  constructors: Seq[TypeConstructor],
  docs: Seq[Documentation]
) extends TypeLike with Documented

case class ExternalType(
  parent: String,
  name: String,
  typeArguments: Seq[TypeParameter]
) extends TypeLike {
  override def fullName = name
  def parents = Seq.empty
}

case class TypeParameter(
  name: String,
  typeParameters: Seq[TypeParameter]
)

case class TypeAlias(
  parent: String,
  name: String,
  `type`: TypeLike
) extends TypeLike {
  def parents = Seq.empty
}

case class TypeConstructor(
  parent: String,
  name: String,
  id: Int,
  typeArguments: Seq[TypeParameter],
  arguments: Seq[Argument],
  docs: Seq[Documentation]
) extends NamedElement with TypeId with Documented {
  def packageName = parent
}

case class TypeStatement(
  typeReference: TypeReference,
  typeArguments: Seq[TypeStatement],
  isTypeArgument: Boolean
)

case class TypeReference(
  fullName: String
)

case class Call(
  parent: String,
  name: String,
  id: Int,
  arguments: Seq[Argument],
  returnType: TypeStatement,
  parents: Seq[Trait],
  docs: Seq[Documentation]
) extends TypeLike with TypeId with Documented

case class Trait(
  parent: String,
  name: String,
  parents: Seq[Trait],
  docs: Seq[Documentation]
) extends TypeLike with Documented

trait PackageLike extends Definition {
  def name: String
  def definitions: Seq[Definition]

  def deepDefinitions: Seq[Definition] = {
    definitions ++ (definitions flatMap {
      case pl: PackageLike => pl.deepDefinitions
      case _ => Seq.empty
    })
  }

}

case class Package(
  parent: String,
  name: String,
  definitions: Seq[Definition]
) extends PackageLike {
}

case class SchemaVersion(
  parent: String,
  version: Int,
  definitions: Seq[Definition]
) extends PackageLike {
  def name = "v" + version
}

case class Schema(
  name: String,
  versions: Seq[SchemaVersion]
) extends PackageLike {
  def definitions = versions
  def parent = ""

  def latestVersion: SchemaVersion = {
    versions.maxBy(_.version)
  }

}

case class Model(
  schemas: Seq[Schema],
  private[astparser] val parsedModel: parser.Model
) {

  private val definitions = deepDefinitions.map { d =>
    d.fullName -> d
  }.toMap

  def deepDefinitions: Seq[Definition] = {
    schemas ++ (schemas flatMap (_.deepDefinitions))
  }

  def getDefinition(fullName: String): Option[Definition] = {
    definitions.get(fullName)
  }

  def getType(typeReference: TypeReference): TypeLike = {
    getDefinition(typeReference.fullName) match {
      case Some(t: TypeLike) => t
      case x =>
        throw new Exception(s"found unexpected entity for $typeReference: $x")
    }
  }

}


object Model {
  private val astParser = new AstParser()

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
          s.versions.map { v =>
            SchemaVersion(
              parent = s.name,
              version = v.version,
              definitions = convertDefinitions(v.definitions)
            )
          })
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
      t.name,
      t.parents map resolveTrait map convertTrait,
      t.docs map convertDocs
    )
  }

  private def convertCall(
    c: parser.Call
  )(implicit m: parser.Model): Call = {
    Call(
      c.packageName,
      c.name,
      c.id,
      c.arguments map convertArgument,
      convertTypeStatement(c.returnType),
      c.parents map resolveTrait map convertTrait,
      c.docs map convertDocs
    )
  }

  private def convertArgument(
    a: parser.Argument
  )(implicit m: parser.Model): Argument = {
    Argument(
      a.name,
      convertTypeStatement(a.`type`),
      a.docs map convertDocs
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
      case t: parser.Trait =>
        convertTrait(t)
      case ta: parser.TypeAlias =>
        convertTypeAlias(ta)
      case et: parser.ExternalType =>
        convertExternalType(et)
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
    val ta = t.typeArguments map convertTypeParameter
    Type(
      t.packageName,
      name = t.name,
      typeArguments = ta,
      parents = t.parents map resolveTrait map convertTrait,
      constructors = t.constructors map convertTypeConstructor,
      docs = t.docs map convertDocs
    )
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
      case Some(t: parser.Type) =>
        t
      case Some(et: parser.ExternalType) =>
        et
      case Some(t: parser.Trait) =>
        t
      case Some(ta: parser.TypeAlias) =>
        ta
      case Some(i: parser.Import) =>
        resolve(i.reference)
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
      docs = c.docs map convertDocs
    )
  }

  private def convertDocs(
    d: parser.Documentation
  )(implicit m: parser.Model): Documentation = {
    Documentation(d.content)
  }

}
