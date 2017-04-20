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
    this.toString + s" defined at $pos"
  }

  def initParents(): Unit = {
    children.foreach { c =>
      c.initParents()
      c.maybeParent = Some(this)
    }
  }

}

private[astparser] case class VersionsInterval(
    start: Option[Int],
    end: Option[Int]
) extends Element

private[astparser] sealed trait Versioned {
  def versions: VersionsInterval
}

private[astparser] trait TypeId extends Element {
  def maybeId: Option[Int]
  def idString: String
  def id: Int = maybeId.getOrElse {
    val r = CRC32Helper.crc32(idString)
    //println(f"computed hash `$r%02x` for `$fixedId` (before fix `$idString`)")
    r
  }
  def idHex: String = f"$id%02x"

  override def humanReadable: String = {
    idString + s" defined at $pos"
  }

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

private[astparser] sealed trait Argumented {
  def arguments: Seq[Argument]
}

private[astparser] final case class Type(
    name: String,
    typeArguments: Seq[TypeParameter],
    parents: Seq[Reference],
    constructors: Seq[TypeConstructor],
    docs: Seq[Documentation]
) extends TypeLike
    with PackageLike {
  children = constructors ++ typeArguments ++ parents ++ docs
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
    versions: VersionsInterval,
    docs: Seq[Documentation]
) extends TypeLike
    with TypeId
    with Argumented
    with Versioned {
  children = typeArguments ++ arguments ++ parents ++ docs
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
        throw new Exception(
          s"parent of constructor should be `Type`, but $x found")
    }

  }
}

private[astparser] final case class Reference(
    fullName: String
) extends NamedElement {
  def name = fullName.simpleName
  assert(name.nonEmpty, "reference fullName couldn't be empty")

  override def toString: String = fullName
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
    httpRequest: Option[String],
    versions: VersionsInterval,
    docs: Seq[Documentation]
) extends TypeLike
    with TypeId
    with Versioned
    with Argumented {
  children = (arguments :+ returnType) ++ parents ++ docs
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
) extends NamedElement
    with Documented {
  children = `type` +: docs

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
          throw new Exception(
            s"couldn't generate idString for ${this.humanReadable}")
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
          case Some(i: Import) =>
            log.error(s"found $i")
            lookup(i.reference) match {
              case Some(p: PackageLike) =>
                log.error(s"found $p at import, rest: $rest")
                p.getDefinition(rest)
              case Some(x) =>
                throw new Exception(s"Unexpected $x, PackageLike expected")
              case None =>
                log.error("import not found")
                None
            }
          case Some(x) =>
            throw new Exception(s"Unexpected $x, PackageLike expected")
          case None =>
            None
        }
    }
  }

  final def lookup(ref: Reference): Option[Definition] =
    loggingTime("lookup") {
      def lookupInScope(packageName: String): Option[Definition] = {
        getDefinition(packageName) match {
          case Some(p: PackageLike) =>
            p.getDefinition(ref) match {
              case found @ Some(_) =>
                found
              case None =>
                p.maybePackage match {
                  case Some(_) =>
                    lookupInScope(p.packageName)
                  case None =>
                    getDefinition(ref)
                }
            }
          case Some(x) =>
            throw new Exception(
              s"expected package for name `$packageName`, found ${x.humanReadable}")
          case None =>
            getDefinition(ref) match {
              case found @ Some(_) => found
              case None =>
                maybePackage flatMap (_.lookup(ref))
            }
        }
      }

      lookupInScope(ref.packageName)
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
    arguments: Seq[Argument],
    parents: Seq[Reference],
    docs: Seq[Documentation]
) extends TypeLike
    with Argumented {
  children = parents ++ arguments ++ docs
}

private[astparser] final case class Documentation(
    content: String
) extends Element

trait Documented {
  def docs: Seq[Documentation]
}

private[astparser] final case class Schema(
    name: String,
    definitions: Seq[Definition]
) extends PackageLike {
  children = definitions
  //def name: String = ""
  def fullName = name
}

private[astparser] final case class Model(
    private val _schemas: Seq[Schema]
) extends PackageLike
    with Logger {
  val schemas = _schemas
    .groupBy(_.name)
    .map {
      case (name, schemas) =>
        Schema(name, schemas.flatMap(_.definitions))
    }
    .toSeq

  children = schemas
  initParents()

  validate()

  def definitions = schemas
  def fullName = ""
  def name = ""

  def findSchema(name: String): Option[Schema] = {
    schemas.find(_.name == name)
  }

  private[astparser] def validate(): Unit = loggingTime("validateModel") {
    val c = schemas.flatMap(allChildren)
    log.info(s"validating model containing ${c.size} elements")
    val allReferences = c.collect {
      case r: Reference => r
    } ++ deepDefinitions.collect {
      case t: TypeLike => t.parents
    }.flatten
    val unresolvedReferences = allReferences.filter(lookup(_).isEmpty)

    if (unresolvedReferences.nonEmpty) {
      failValidation(
        s"model contains unresolved references:\n" +
          unresolvedReferences.map(_.humanReadable).mkString("\n")
      )
    }

    val duplicateIds = schemas flatMap { s =>
      s.deepDefinitions
        .collect {
          case ti: TypeId => ti.id -> ti
        }
        .groupBy { case (id, t) => id }
        .filter { case (id, types) => types.size > 1 }
        .flatMap { case (id, types) => types.map(_._2) }
    }

    if (duplicateIds.nonEmpty) {
      failValidation(
        s"Model contains duplicate ids:" + System.lineSeparator() +
          duplicateIds.map(_.humanReadable).mkString(System.lineSeparator())
      )
    }

    val duplicateDefinitions = deepDefinitions
      .map { definition =>
        val duplicates = definition.children
          .collect {
            case ne: NamedElement => ne
          }
          .groupBy(_.name)
          .filter { case (name, elems) => elems.size > 1 }
          .map { case (name, elems) => elems }
          .filter { elems =>
            !elems.forall(_.isInstanceOf[TypeConstructor]) &&
            !elems.forall(_.isInstanceOf[Call])
          }
        definition -> duplicates
      }
      .filter { case (d, duplicates) => duplicates.nonEmpty }

    if (duplicateDefinitions.nonEmpty) {
      failValidation(
        "Model contains duplicate definitions:" + System.lineSeparator() +
          duplicateDefinitions
            .map {
              case (d, duplicates) =>
                duplicates.flatten
                  .map(_.humanReadable)
                  .mkString(System.lineSeparator())
            }
            .mkString(System.lineSeparator())
      )
    }

    val traits = deepDefinitions.collect {
      case t: Trait => t
    }
    def hasParent(t: Trait, parents: Seq[Reference]): Boolean = {
      parents.exists { ref =>
        lookup(ref).get == t
      }
    }
    val typeWithMissedField = traits map { t =>
      val inheritors = deepDefinitions.collect {
        case tl: TypeLike with Argumented if hasParent(t, tl.parents) =>
          Seq(tl)
        case aType: Type if hasParent(t, aType.parents) => aType.constructors
      }.flatten
      t -> inheritors
    } filter {
      case (t, inheritors) =>
        !t.arguments.forall { a =>
          inheritors
            .forall(_.arguments.map(_.name).contains(a.name)) //check type here?
        }
    }

    if (typeWithMissedField.nonEmpty) {
      failValidation(
        "Models contains trait inheritors with missed fields:" + System
          .lineSeparator() +
          typeWithMissedField
            .map {
              case (t, inheritors) =>
                t.humanReadable + ": " + inheritors
                  .map(_.humanReadable)
                  .mkString("")
            }
            .mkString(System.lineSeparator())
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
