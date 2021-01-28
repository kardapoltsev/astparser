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

import com.github.kardapoltsev.astparser.util.{CRC32Helper, Logger}
import com.github.kardapoltsev.astparser.util.StringUtil._
import scala.util.parsing.input.Positional

sealed private[astparser] trait Element extends Positional with Logger {
  var maybeParent: Option[Element]                = None
  protected[astparser] var children: Seq[Element] = Seq.empty

  def maybeSchema: Option[Schema] = {
    this match {
      case s: Schema =>
        Some(s)
      case _ =>
        maybeParent match {
          case Some(s: Schema) => Some(s)
          case Some(element)   => element.maybeSchema
          case None            => None
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
      case Some(element)        => element.maybePackage
      case None                 => None
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

private[astparser] case class EnableConstraint(
  constraints: Seq[String]
) extends Element

private[astparser] case class DisableConstraint(
  constraints: Seq[String]
) extends Element

private[astparser] case class Constraint(
  enable: EnableConstraint,
  disable: DisableConstraint
) extends Element

sealed private[astparser] trait Constrained {
  def constraint: Constraint
  assert(
    constraint.enable.constraints.intersect(constraint.disable.constraints).isEmpty,
    "enable and disable contains the same constraint"
  )
}

private[astparser] case class VersionsInterval(
  start: Option[Int],
  end: Option[Int]
) extends Element

sealed private[astparser] trait Versioned {
  def versions: VersionsInterval
}

private[astparser] trait TypeId extends Element {
  def maybeId: Option[Int]
  def idString: String
  def id: Int = maybeId.getOrElse {
    CRC32Helper.crc32(idString)
  }
  def idHex: String = f"$id%02x"

  override def humanReadable: String = {
    idString + s" defined at $pos"
  }

}

sealed private[astparser] trait NamedElement extends Element {
  def name: String
  def fullName: String
}

sealed private[astparser] trait Definition extends NamedElement

sealed private[astparser] trait TypeLike extends Definition with Documented {
  def parents: Seq[Reference]
  def fullName = packageName ~ name
}

sealed private[astparser] trait Argumented {
  def arguments: Seq[Argument]
}

final private[astparser] case class Type(
  name: String,
  typeArguments: Seq[TypeParameter],
  parents: Seq[Reference],
  constructors: Seq[TypeConstructor],
  docs: Seq[Documentation],
  constraint: Constraint = Constraint(EnableConstraint(Nil), DisableConstraint(Nil))
) extends TypeLike
    with PackageLike
    with Constrained {
  children = constructors ++ typeArguments ++ parents ++ docs
  def definitions = constructors
  def isGeneric   = typeArguments.nonEmpty
}

final private[astparser] case class ExternalType(
  override val fullName: String,
  typeArguments: Seq[TypeParameter],
  constraint: Constraint = Constraint(EnableConstraint(Nil), DisableConstraint(Nil))
) extends TypeLike
    with Constrained {
  def docs    = Seq.empty
  def parents = Seq.empty
  def name    = fullName.simpleName
}

final private[astparser] case class TypeParameter(
  name: String,
  typeParameters: Seq[TypeParameter]
) extends Element

final private[astparser] case class TypeConstructor(
  name: String,
  maybeId: Option[Int],
  typeArguments: Seq[TypeParameter],
  arguments: Seq[Argument],
  parents: Seq[Reference],
  versions: VersionsInterval,
  docs: Seq[Documentation],
  constraint: Constraint = Constraint(EnableConstraint(Nil), DisableConstraint(Nil))
) extends TypeLike
    with TypeId
    with Argumented
    with Versioned
    with Constrained {
  children = typeArguments ++ arguments ++ parents ++ docs
  def idString: String = {
    maybeParent match {
      case Some(t: Type) =>
        val argString =
          if (arguments.isEmpty) ""
          else " " + arguments.map(_.idString).mkString(" ")

        val packageName = t.packageName
        val fullName    = t.fullName
        "type " + packageName ~ name + argString + " = " + fullName
      case x =>
        throw new Exception(s"parent of constructor should be `Type`, but $x found")
    }

  }
}

final private[astparser] case class Reference(
  fullName: String
) extends NamedElement {
  def name = fullName.simpleName
  assert(name.nonEmpty, "reference fullName couldn't be empty")

  override def toString: String = fullName
}

final private[astparser] case class Import(
  name: String,
  reference: Reference,
  constraint: Constraint = Constraint(EnableConstraint(Nil), DisableConstraint(Nil))
) extends Definition
    with Constrained {
  children = Seq(reference)
  def fullName = packageName ~ name
}

final private[astparser] case class TypeAlias(
  name: String,
  `type`: TypeStatement,
  constraint: Constraint = Constraint(EnableConstraint(Nil), DisableConstraint(Nil))
) extends TypeLike
    with Constrained {
  children = Seq(`type`)
  def docs    = Seq.empty
  def parents = Seq.empty
}

final private[astparser] case class Call(
  name: String,
  maybeId: Option[Int],
  arguments: Seq[Argument],
  returnType: TypeStatement,
  parents: Seq[Reference],
  httpRequest: Option[String],
  versions: VersionsInterval,
  docs: Seq[Documentation],
  constraint: Constraint = Constraint(EnableConstraint(Nil), DisableConstraint(Nil))
) extends TypeLike
    with TypeId
    with Versioned
    with Argumented
    with Constrained {
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

final private[astparser] case class Argument(
  name: String,
  `type`: TypeStatement,
  docs: Seq[Documentation]
) extends NamedElement
    with Documented {
  children = `type` +: docs

  def fullName: String = name
  def idString: String = s"$name:${`type`.idString}"
}

final private[astparser] case class TypeStatement(
  ref: Reference,
  typeArguments: Seq[TypeStatement] = Seq.empty
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
        case Seq(i: Import) =>
          resolve(i.reference)
        case found if found.nonEmpty =>
          val pn = found.head.packageName
          val n  = found.head.name
          assume(found.forall(_.packageName == pn), s"${r.humanReadable} resolved to unexpected $found")
          assume(found.forall(_.name == n), s"${r.humanReadable} resolved to unexpected $found")
          found.head
        case _ =>
          log.error(s"unable to lookup ${r.humanReadable}")
          throw new Exception(s"couldn't generate idString for ${this.humanReadable}")
      }
    }
    val resolved = resolve(ref)
    val fullName = resolved.packageName ~ resolved.name
    s"${fullName}$argStr"
  }
}

sealed private[astparser] trait PackageLike extends Definition with Logger {
  def definitions: Seq[Definition]

  def deepDefinitions: Seq[Definition] = {
    definitions flatMap {
      case p: PackageLike => p +: p.deepDefinitions
      case d              => Seq(d)
    }
  }

  def getDefinition(ref: Reference): Seq[Definition] = {
    getDefinition(ref.fullName.toPath)
  }

  def getDefinition(fullName: String): Seq[Definition] = {
    getDefinition(fullName.toPath)
  }

  final protected def getDefinition(path: List[String]): Seq[Definition] = {
    path match {
      case Nil =>
        Seq.empty //or throw?
      case name :: Nil =>
        definitions.filter(_.name == name)
      case name :: rest =>
        definitions.find(_.name == name) match {
          case Some(p: PackageLike) =>
            p.getDefinition(rest)
          case Some(i: Import) =>
            lookup(i.reference) match {
              case Seq(p: PackageLike) =>
                p.getDefinition(rest)
              case x if x.nonEmpty =>
                throw new Exception(s"Unexpected $x, PackageLike expected")
              case empty =>
                empty
            }
          case Some(x) =>
            throw new Exception(s"Unexpected $x, PackageLike expected")
          case None =>
            Seq.empty
        }
    }
  }

  final def lookup(ref: Reference): Seq[Definition] =
    loggingTime("lookup") {
      def lookupInScope(packageName: String): Seq[Definition] = {
        getDefinition(packageName).headOption match {
          case Some(p: PackageLike) =>
            p.getDefinition(ref) match {
              case found if found.nonEmpty =>
                found
              case _ =>
                p.maybePackage match {
                  case Some(_) =>
                    lookupInScope(p.packageName)
                  case None =>
                    getDefinition(ref)
                }
            }
          case Some(x) =>
            throw new Exception(s"expected package for name `$packageName`, found ${x.humanReadable}")
          case None =>
            getDefinition(ref) match {
              case found if found.nonEmpty => found
              case _ =>
                maybePackage.toSeq flatMap (_.lookup(ref))
            }
        }
      }

      lookupInScope(ref.packageName)
    }

}

final private[astparser] case class Package(
  name: String,
  definitions: Seq[Definition],
  constraint: Constraint = Constraint(EnableConstraint(Nil), DisableConstraint(Nil))
) extends PackageLike
    with Constrained {
  children = definitions
  def fullName = packageName ~ name
}

final private[astparser] case class Trait(
  name: String,
  arguments: Seq[Argument],
  parents: Seq[Reference],
  docs: Seq[Documentation],
  constraint: Constraint = Constraint(EnableConstraint(Nil), DisableConstraint(Nil))
) extends TypeLike
    with Argumented
    with Constrained {
  children = parents ++ arguments ++ docs
}

final private[astparser] case class Documentation(
  content: String
) extends Element

trait Documented {
  def docs: Seq[Documentation]
}

final private[astparser] case class Schema(
  name: String,
  definitions: Seq[Definition],
  constraint: Constraint = Constraint(EnableConstraint(Nil), DisableConstraint(Nil))
) extends PackageLike
    with Constrained {
  children = definitions
  //def name: String = ""
  def fullName = name
}

final private[astparser] case class Model(
  private val _schemas: Seq[Schema]
) extends PackageLike
    with Logger {
  val schemas = _schemas
    .groupBy(_.name)
    .map {
      case (name, schemas) =>
        val enable  = schemas.flatMap(_.constraint.enable.constraints).distinct
        val disable = schemas.flatMap(_.constraint.disable.constraints).distinct
        val constraint =
          Constraint(enable = EnableConstraint(enable), disable = DisableConstraint(disable))
        Schema(name, schemas.flatMap(_.definitions), constraint)
    }
    .toSeq

  children = schemas
  initParents()

  validate()

  def definitions = schemas
  def fullName    = ""
  def name        = ""

  def findSchema(name: String): Option[Schema] = {
    schemas.find(_.name == name)
  }

  private[astparser] def validate(): Unit = loggingTime("validateParserModel") {
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
      s.deepDefinitions.collect {
        case ti: TypeId => ti.id -> ti
      }.groupBy { case (id, t) => id }.filter { case (id, types) => types.size > 1 }.flatMap {
        case (id, types) => types.map(_._2)
      }
    }

    if (duplicateIds.nonEmpty) {
      failValidation(
        s"Model contains duplicate ids:" + System.lineSeparator() +
          duplicateIds.map(_.humanReadable).mkString(System.lineSeparator())
      )
    }

    val selfInheritance = schemas flatMap { s =>
      s.deepDefinitions.collect {
        case t: TypeLike if t.parents.exists { p =>
              (t.packageName ~ p.fullName) == t.fullName || p.fullName == t.fullName
            } =>
          t
      }
    }
    if (selfInheritance.nonEmpty) {
      failValidation(
        s"Model contains self-inherited elements: ${selfInheritance.map(_.humanReadable).mkString(System.lineSeparator())}"
      )
    }

    val duplicateDefinitions = deepDefinitions.map { definition =>
      val duplicates = definition.children.collect {
        case ne: NamedElement => ne
      }.groupBy(_.name).filter { case (name, elems) => elems.size > 1 }.map { case (name, elems) => elems }.filter {
        elems =>
          !elems.forall(_.isInstanceOf[TypeConstructor]) &&
          !elems.forall(_.isInstanceOf[Call])
      }
      definition -> duplicates
    }.filter { case (_, duplicates) => duplicates.nonEmpty }

    if (duplicateDefinitions.nonEmpty) {
      failValidation(
        "Model contains duplicate definitions:" + System.lineSeparator() +
          duplicateDefinitions.map {
            case (d, duplicates) =>
              duplicates.flatten
                .map(_.humanReadable)
                .mkString(System.lineSeparator())
          }.mkString(System.lineSeparator())
      )
    }

    val traits = deepDefinitions.collect {
      case t: Trait => t
    }
    def hasParent(t: Trait, parents: Seq[Reference]): Boolean = {
      parents.exists { ref =>
        lookup(ref).contains(t)
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
          typeWithMissedField.map {
            case (t, inheritors) =>
              t.humanReadable + ": " + inheritors
                .map(_.humanReadable)
                .mkString(", ")
          }.mkString(System.lineSeparator())
      )
    }

  }

  private def failValidation(msg: String): Unit = {
    log.error(msg)
    throw new ParserValidationException(msg)
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
