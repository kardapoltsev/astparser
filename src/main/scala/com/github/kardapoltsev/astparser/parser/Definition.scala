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

import com.github.kardapoltsev.astparser.util.{CRC32Helper, Logger}
import com.github.kardapoltsev.astparser.util.StringUtil._
import scala.util.parsing.input.Positional


private[astparser] sealed trait Element extends Positional with Logger {
  var maybeParent: Option[Element] = None
  protected[astparser] var children: Seq[Element] = Seq.empty

  def maybeSchema: Option[Schema] = {
    this match {
      case s: Schema =>
        Some(s)
      case _ =>
        maybeParent match {
          case Some(s: Schema) => Some(s)
          case Some(element) => element.maybeSchema
          case None => None
        }
    }
  }

  def schema: Schema = {
    maybeSchema match {
      case Some(s) => s
      case None =>
        throw new Exception(s"Schema not found for ${this.humanReadable}")
    }
  }

  def maybeSchemaVersion: Option[SchemaVersion] = {
    this match {
      case v: SchemaVersion =>
        Some(v)
      case _ =>
        maybeParent match {
          case Some(v: SchemaVersion) => Some(v)
          case Some(e) => e.maybeSchemaVersion
          case None => None
        }
    }
  }

  def schemaVersion: SchemaVersion = {
    maybeSchemaVersion match {
      case Some(sv) => sv
      case None =>
        throw new Exception(s"SchemaVersion not found for ${this.humanReadable}")
    }
  }

  def maybePackage: Option[PackageLike] = {
    maybeParent match {
      case Some(p: PackageLike) => Some(p)
      case Some(element) => element.maybePackage
      case None => None
    }
  }

  def packageName: String = {
    maybePackage match {
      case Some(p) =>
        p.packageName ~ p.name
      case None => ""
    }
  }

  def humanReadable: String = {
    this.toString.take(80) + s" defined at $pos"
  }

  def initParents(): Unit = {
    children.foreach { c =>
      c.initParents()
      c.maybeParent = Some(this)
    }
  }

}

private[astparser] trait TypeId {
  def maybeId: Option[Int]
  def idString: String
  def id: Int = maybeId.getOrElse {
    //remove versions (api.v1 -> api) from idString
    val fixedId = idString.replaceAll(s"v\\d+.", "")
    val r = CRC32Helper.crc32(fixedId)
    //println(f"computed hash `$r%02x` for `$fixedId` (before fix `$idString`)")
    r
  }
  def idHex: String = f"$id%02x"
}

private[astparser] sealed trait NamedElement extends Element {
  def name: String
  def fullName: String
}

private[astparser] sealed trait Definition extends NamedElement

private[astparser] sealed trait TypeLike extends Definition with Documented {
  def parents: Seq[Reference]
  def fullName = packageName ~ name
}

private[astparser] final case class Type(
  name: String,
  typeArguments: Seq[TypeParameter],
  parents: Seq[Reference],
  constructors: Seq[TypeConstructor],
  docs: Seq[Documentation]
) extends TypeLike {
  children = constructors ++ typeArguments ++ parents
  def definitions = constructors
  def isGeneric = typeArguments.nonEmpty
}

private[astparser] final case class ExternalType(
  override val fullName: String,
  typeArguments: Seq[TypeParameter]
) extends TypeLike {
  def docs = Seq.empty
  def parents = Seq.empty
  def name = fullName.simpleName
}

private[astparser] final case class TypeParameter(
  name: String,
  typeParameters: Seq[TypeParameter]
) extends Element

private[astparser] final case class TypeConstructor(
  name: String,
  maybeId: Option[Int],
  typeArguments: Seq[TypeParameter],
  arguments: Seq[Argument],
  parents: Seq[Reference],
  docs: Seq[Documentation]
) extends NamedElement with TypeId with Documented {
  children = typeArguments ++ arguments
  def fullName: String = packageName ~ name
  def idString: String = {
    maybeParent match {
      case Some(t: Type) =>
        val argString =
          if (arguments.isEmpty) ""
          else " " + arguments.map(_.idString).mkString(" ")

        val packageName = t.packageName
        val fullName = t.fullName
        "type " + packageName ~ name + argString + " = " + fullName
      case x =>
        throw new Exception(s"parent of constructor should be `Type`, but $x found")
    }

  }
}


private[astparser] final case class Reference(
  fullName: String
) extends NamedElement {
  def name = fullName.simpleName
  assert(name.nonEmpty, "reference fullName couldn't be empty")
}

private[astparser] final case class Import(
  name: String,
  reference: Reference
) extends Definition {
  children = Seq(reference)
  def fullName = packageName ~ name
}


private[astparser] final case class TypeAlias(
  name: String,
  reference: Reference
) extends TypeLike {
  children = Seq(reference)
  def docs = Seq.empty
  def parents = Seq.empty
  //def fullName = packageName ~ name
}

private[astparser] final case class Call(
  name: String,
  maybeId: Option[Int],
  arguments: Seq[Argument],
  returnType: TypeStatement,
  parents: Seq[Reference],
  docs: Seq[Documentation]
) extends TypeLike with TypeId {
  children = (arguments :+ returnType) ++ parents
  def idString: String = {
    val packageNamePrefix =
      if (packageName.isEmpty) ""
      else packageName + "."

    val argString =
      if (arguments.isEmpty) ""
      else " " + arguments.map(_.idString).mkString(" ")

    "call " + packageNamePrefix + name + argString + " = " + returnType.idString
  }
}

private[astparser] final case class Argument(
  name: String,
  `type`: TypeStatement,
  docs: Seq[Documentation]
) extends NamedElement with Documented {
  children = Seq(`type`)

  def fullName: String = name
  def idString: String = s"$name:${`type`.idString}"
}

private[astparser] final case class TypeStatement(
  ref: Reference,
  typeArguments: Seq[TypeStatement]
) extends Element {
  children = ref +: typeArguments
  def idString: String = {
    val argStr =
      if (typeArguments.isEmpty) ""
      else "[" + typeArguments.map(_.idString).mkString(" ") + "]"

    //TODO: fix idString
    val m = schema.maybeParent.get.asInstanceOf[Model]
    def resolve(r: Reference): Definition = {
      m.lookup(r) match {
        //case a: TypeAlias =>
        //  resolve(a.reference)
        case Some(i: Import) =>
          resolve(i.reference)
        case Some(d) => d
        case None =>
          log.error("unable to lookup {}", r.humanReadable)
          throw new Exception(s"couldn't generate idString for ${this.humanReadable}")
      }
    }
    val resolved = resolve(ref)
    val fullName = resolved.packageName ~ resolved.name
    s"${fullName}$argStr"
  }
}

private[astparser] sealed trait PackageLike extends Definition with Logger {
  def definitions: Seq[Definition]

  def deepDefinitions: Seq[Definition] = {
    definitions flatMap {
      case p: PackageLike => p +: p.deepDefinitions
      case d => Seq(d)
    }
  }

  def getDefinition(ref: Reference): Option[Definition] = {
    getDefinition(ref.fullName.toPath)
  }

  def getDefinition(fullName: String): Option[Definition] = {
    getDefinition(fullName.toPath)
  }

  final protected def getDefinition(path: List[String]): Option[Definition] = {
    path match {
      case Nil =>
        None //or throw?
      case name :: Nil =>
        definitions.find(_.name == name)
      case name :: rest =>
        definitions.find(_.name == name) match {
          case Some(p: PackageLike) =>
            p.getDefinition(rest)
          case Some(x) =>
            throw new Exception(s"Unexpected $x, PackageLike expected")
          case None =>
            None
        }
    }
  }

}

private[astparser] final case class Package(
  name: String,
  definitions: Seq[Definition]
) extends PackageLike {
  children = definitions
  def fullName = packageName ~ name
}

private[astparser] final case class Trait(
  name: String,
  parents: Seq[Reference],
  docs: Seq[Documentation]
) extends TypeLike {
  children = parents
}

private[astparser] final case class Documentation(
  content: String
) extends Positional

trait Documented {
  def docs: Seq[Documentation]
}

private[astparser] final case class SchemaVersion(
  version: Int,
  definitions: Seq[Definition]
) extends PackageLike {
  children = definitions
  def name = s"v$version"
  def fullName = packageName ~ name
}

private[astparser] final case class Schema(
  name: String,
  versions: Seq[SchemaVersion]
) extends PackageLike {
  children = definitions
  def definitions = versions
  //def name: String = ""
  def fullName = name
}


private[astparser] final case class Model(
  private val _schemas: Seq[Schema]
) extends PackageLike with Logger {
  val schemas = _schemas.groupBy(_.name).map { case (name, versions) =>
    Schema(name, versions.flatMap(_.versions))
  }.toSeq
  //schemas.foreach(_.initParents())
  children = schemas
  initParents()

  validate()

  def definitions = schemas
  def fullName = ""
  def name = ""

  def findSchema(name: String): Option[Schema] = {
    schemas.find(_.name == name)
  }

  def isLatest(version: SchemaVersion): Boolean = {
    !version.schema.versions.exists(_.version > version.version)
  }

  def maybeNewerSchema(version: SchemaVersion): Option[SchemaVersion] = {
    version.schema.versions.find(_.version == version.version + 1)
  }


  private def maybeNewerVersion(fullName: String): Option[String] = {
    fullName.split("\\.").toList match {
      case schema :: version :: rest if version.startsWith("v") =>
        findSchema(schema) flatMap { s =>
          val currentVersion = version.drop(1).toInt
          s.versions.sortBy(_.version).find(_.version > currentVersion) map { nv =>
            schema ~ s"v${nv.version}" ~ rest.mkString(".")
          }
        }
      case _ => None
    }
  }


  def lookup(ref: Reference): Option[Definition] = loggingTime("lookup") {
    def lookupAbsolute(fullName: String): Option[Definition] = {
      getDefinition(fullName) match {
        case found @ Some(_) => found
        case None =>
          maybeNewerVersion(fullName) flatMap { newer =>
            lookupAbsolute(newer)
          }
      }
    }

    def lookupVersioned(packageName: String): Option[Definition] = {
      getDefinition(packageName) match {
        case Some(p: PackageLike) =>
          p.getDefinition(ref) match {
            case found @ Some(_) =>
              found
            case None =>
              maybeNewerVersion(packageName) match {
                case Some(newer) =>
                  lookupVersioned(newer)
                case None =>
                  None
              }
          }
        case Some(x) =>
          throw new Exception(s"expected package for name `$packageName`, found ${x.humanReadable}")
        case None =>
          maybeNewerVersion(packageName) match {
            case Some(newer) =>
              lookupVersioned(newer)
            case None =>
              None
          }
      }
    }

    def lookupInScope(packageName: String): Option[Definition] = {
      lookupVersioned(packageName) match {
        case found @ Some(_) =>
          found
        case None =>
          getDefinition(packageName) match {
            case Some(p: PackageLike) =>
              p.maybePackage match {
                case Some(outer) =>
                  lookupInScope(p.packageName)
                case None =>
                  lookupAbsolute(ref.fullName)
              }
            case Some(x) =>
              throw new Exception(s"expected package for name `$packageName`, found ${x.humanReadable}")
            case None =>
              lookupAbsolute(ref.fullName)
          }
      }
    }

    lookupInScope(ref.packageName)
  }

  private[astparser] def validate(): Unit = loggingTime("validateModel") {
    val c = schemas.flatMap(allChildren)
    log.info(s"validating model containing ${c.size} elements")
    val unresolvedReferences = c.collect {
      case r: Reference if lookup(r).isEmpty => r
    }

    if(unresolvedReferences.nonEmpty) {
      failValidation(
        s"model contains unresolved references:\n" +
          unresolvedReferences.map(_.humanReadable).mkString("\n")
      )
    }

    val duplicateIds = schemas flatMap { s =>
      s.versions flatMap { v =>
        v.deepDefinitions.collect {
          case ti: TypeId => ti.id -> ti
        }.groupBy { case (id, t) => id }.
          filter { case (id, types) => types.size > 1 }.
          flatMap { case (id, types) => types.map(_._2) }
      }
    }

    if(duplicateIds.nonEmpty) {
      failValidation(
        s"Model contains duplicate ids:" + System.lineSeparator() +
          duplicateIds.map(_.humanReadable).mkString(System.lineSeparator())
      )
    }

    val duplicateDefinitions = schemas.flatMap(_.deepDefinitions).map { definition =>
      val duplicates = definition.children.collect {
        case ne: NamedElement => ne
      }.groupBy(_.name).filter { case (name, elems) => elems.size > 1 }.
        map { case (name, elems) => elems}
      definition -> duplicates
    }.filter { case (d, duplicates) => duplicates.nonEmpty}

    if(duplicateDefinitions.nonEmpty) {
      failValidation(
        "Model contains duplicate definitions:" + System.lineSeparator() +
        duplicateDefinitions.map { case (d, duplicates) =>
          duplicates.map { sameElems =>
            s"${sameElems.map(_.humanReadable)} defined at ${d.humanReadable}"
          }.mkString(System.lineSeparator())
        }.mkString(System.lineSeparator())
      )
    }
  }

  private def failValidation(msg: String): Unit = {
    log.error(msg)
    throw new ModelValidationException(msg)
  }


  private def allChildren(e: Element): Seq[Element] = {
    e.children ++ e.children.flatMap(allChildren)
  }

}


object Model {

  private val parser = new AstParser()
  def load(in: Seq[java.io.File]): Model = {
    val schemes = in map { f =>
      parser.parse(f)
    }
    Model(schemes)
  }
}
