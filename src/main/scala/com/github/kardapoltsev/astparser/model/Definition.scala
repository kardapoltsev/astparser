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

import com.github.kardapoltsev.astparser.parser.http.HttpRequest
import com.github.kardapoltsev.astparser.util.StringUtil._

sealed trait AstElement {
  def parent: String
  //all elements except schema itself are inside a scheme
  def schemaName = parent.takeWhile(_ != '.')
}

sealed trait NamedElement extends AstElement {
  def packageName: String
  def name: String
  def fullName = packageName ~ name
}

sealed trait Definition extends NamedElement {
  def packageName = parent
}

case class EnableConstraint(
  constraints: Seq[String]
)

case class DisableConstraint(
  constraints: Seq[String]
)

case class Constraint(
  enable: EnableConstraint,
  disable: DisableConstraint
)

sealed trait Constrained {
  def constraint: Constraint

  def isEnabled(enabledFeatures: Seq[String]): Boolean = {
    val enabled  = constraint.enable.constraints.intersect(enabledFeatures)
    val disabled = constraint.disable.constraints.intersect(enabledFeatures)
    assert(enabled.isEmpty || disabled.isEmpty, "enabledFeatures contains both allowed and disallowed")
    (constraint.enable.constraints.isEmpty || enabled.nonEmpty) && disabled.isEmpty
  }

}

sealed trait TypeId {
  def id: Int
  def idHex: String = f"$id%08x"
}

sealed trait Documented {
  def docs: Documentation
}

sealed trait Versioned {
  def versions: VersionsInterval
}

case class VersionsInterval(start: Option[Int], end: Option[Int]) {

  def contains(version: Int): Boolean = {
    start.forall(_ <= version) && end.forall(_ >= version)
  }

  def isIntersect(other: VersionsInterval): Boolean = {
    !intersect(other).isEmpty
  }

  def intersect(other: VersionsInterval): VersionsInterval = {
    val newStart = Seq(start, other.start).flatten.sorted.lastOption
    val newEnd   = Seq(end, other.end).flatten.sorted.headOption
    VersionsInterval(newStart, newEnd)
  }

  def isEmpty: Boolean = {
    (start, end) match {
      case (Some(s), Some(e)) => s > e
      case _                  => false
    }
  }

}

sealed trait DocElement
case class PlainDoc(content: String)                            extends DocElement
case class DocReference(name: String, reference: TypeReference) extends DocElement

case class Documentation(
  content: Seq[DocElement]
)

case class Argument(
  name: String,
  `type`: TypeStatement,
  docs: Documentation
) extends Documented

sealed trait Parent extends TypeLike

sealed trait TypeLike extends Definition {
  def parents: Seq[Parent]

  def isSubtypeOf(other: Parent): Boolean = {
    parents.contains(other) ||
    parents.foldLeft(false) { (isSubtype, p) =>
      isSubtype || p.isSubtypeOf(other)
    }
  }

}

case class Type(
  parent: String,
  name: String,
  typeArguments: Seq[TypeParameter],
  parents: Seq[Parent],
  constructors: Seq[TypeConstructor],
  docs: Documentation,
  constraint: Constraint
) extends TypeLike
    with Constrained
    with PackageLike
    with Documented {
  override def definitions: Seq[Definition] = constructors

  override def slice(interval: VersionsInterval): Type = {
    val filteredConstructors = constructors.flatMap { c =>
      val filteredVersions = c.versions.filter { v =>
        v.versions.isIntersect(interval)
      }
      if (filteredVersions.nonEmpty) {
        Some(c.copy(versions = filteredVersions))
      } else {
        None
      }
    }
    this.copy(constructors = filteredConstructors)
  }

  override def filterConstrained(enabledFeatures: Seq[String]): Option[Type] = {
    if (isEnabled(enabledFeatures)) {
      val filteredConstructors = constructors.flatMap { c =>
        val filteredVersions = c.versions.filter { v =>
          v.isEnabled(enabledFeatures)
        }
        if (filteredVersions.nonEmpty) {
          Some(c.copy(versions = filteredVersions))
        } else {
          None
        }
      }

      if (filteredConstructors.nonEmpty) {
        Some(this.copy(constructors = filteredConstructors))
      } else {
        None
      }
    } else {
      None
    }
  }

}

case class ExternalType(
  parent: String,
  name: String,
  typeArguments: Seq[TypeParameter] = Nil,
  constraint: Constraint
) extends Parent
    with Constrained {
  override def fullName = name
  def parents           = Seq.empty
}

case class TypeParameter(
  name: String,
  typeParameters: Seq[TypeParameter]
)

case class TypeAlias(
  parent: String,
  name: String,
  `type`: TypeStatement,
  constraint: Constraint
) extends TypeLike
    with Constrained {
  def parents = Seq.empty
}

case class TypeConstructor(
  parent: String,
  name: String,
  versions: Seq[TypeConstructorVersion]
) extends TypeLike {
  val typeReference = TypeReference(parent)

  def parents: Seq[Parent] = {
    versions
      .map(_.parents)
      .reduceLeftOption { (p1, p2) =>
        p1.intersect(p2)
      }
      .getOrElse(Seq.empty)
  }

}

case class TypeConstructorVersion(
  parent: String,
  name: String,
  parents: Seq[Parent],
  id: Int,
  typeArguments: Seq[TypeParameter],
  arguments: Seq[Argument],
  versions: VersionsInterval,
  docs: Documentation,
  constraint: Constraint
) extends TypeLike
    with TypeId
    with Documented
    with Constrained
    with Versioned {
  val typeReference = TypeReference(parent)
}

case class TypeStatement(
  parent: String,
  typeReference: TypeReference,
  typeArguments: Seq[TypeStatement],
  isTypeArgument: Boolean
) extends AstElement

case class TypeReference(
  fullName: String
)

case class Call(
  parent: String,
  name: String,
  id: Int,
  arguments: Seq[Argument],
  returnType: TypeStatement,
  parents: Seq[Parent],
  httpRequest: Option[HttpRequest],
  versions: VersionsInterval,
  docs: Documentation,
  constraint: Constraint
) extends TypeLike
    with TypeId
    with Documented
    with Versioned
    with Constrained

case class Trait(
  parent: String,
  arguments: Seq[Argument],
  name: String,
  parents: Seq[Parent],
  docs: Documentation,
  constraint: Constraint
) extends Parent
    with Documented
    with Constrained

sealed trait PackageLike extends Definition with Constrained {
  def name: String
  def definitions: Seq[Definition]

  def deepDefinitions: Seq[Definition] = {
    definitions ++ (definitions flatMap {
      case pl: PackageLike => pl.deepDefinitions
      case _               => Seq.empty
    })
  }

  def slice(interval: VersionsInterval): PackageLike

  protected def sliceInt(interval: VersionsInterval): Seq[Definition] = {
    definitions flatMap {
      case t: Type =>
        val sliced = t.slice(interval)
        if (sliced.constructors.nonEmpty) {
          Some(sliced)
        } else {
          None
        }
      case p: PackageLike =>
        Some(p.slice(interval))
      case c: Call =>
        if (c.versions.isIntersect(interval)) {
          Some(c)
        } else {
          None
        }
      case d: Definition =>
        Some(d)
    }
  }

  def filterConstrained(enabledFeatures: Seq[String]): Option[PackageLike]

  protected def filterConstrainedInt(enabledFeatures: Seq[String]): Seq[Definition] = {
    if (isEnabled(enabledFeatures)) {
      definitions.flatMap {
        case p: PackageLike => p.filterConstrained(enabledFeatures)
        case d: Constrained =>
          if (d.isEnabled(enabledFeatures)) { Some(d) }
          else None
        case c: TypeConstructor =>
          val filteredVersions = c.versions.filter(_.isEnabled(enabledFeatures))
          if (filteredVersions.nonEmpty) {
            Some(c.copy(versions = filteredVersions))
          } else {
            None
          }
      }
    } else {
      Nil
    }
  }

}

case class Package(
  parent: String,
  name: String,
  definitions: Seq[Definition],
  constraint: Constraint
) extends PackageLike {

  override def slice(interval: VersionsInterval): Package = {
    this.copy(
      definitions = sliceInt(interval)
    )
  }

  override def filterConstrained(enabledFeatures: Seq[String]): Option[Package] = {
    if (isEnabled(enabledFeatures)) {
      Some(this.copy(definitions = filterConstrainedInt(enabledFeatures)))
    } else {
      None
    }
  }

}

case class Schema(
  name: String,
  definitions: Seq[Definition],
  constraint: Constraint
) extends PackageLike {
  def parent = ""

  override def schemaName: String = name

  override def slice(interval: VersionsInterval): Schema = {
    this.copy(definitions = sliceInt(interval))
  }

  def filterConstrained(enabledFeatures: Seq[String]): Option[Schema] = {
    if (isEnabled(enabledFeatures)) {
      Some(this.copy(definitions = filterConstrainedInt(enabledFeatures)))
    } else {
      None
    }
  }

}
