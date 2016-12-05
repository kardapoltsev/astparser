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

import com.github.kardapoltsev.astparser.parser.http.HttpRequest
import com.github.kardapoltsev.astparser.util.StringUtil._



sealed trait AstElement {
  def parent: String
}

sealed trait NamedElement extends AstElement {
  def packageName: String
  def name: String
  def fullName = packageName ~ name
}

sealed trait Definition extends NamedElement {
  def parent: String
  def packageName = parent
}

sealed trait TypeId {
  def id: Int
  def idHex: String = f"$id%08x"
}

sealed trait Documented {
  def docs: Seq[Documentation]
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
    val newEnd = Seq(end, other.end).flatten.sorted.headOption
    VersionsInterval(newStart, newEnd)
  }

  def isEmpty: Boolean = {
    (start, end) match {
      case (Some(s), Some(e)) => s > e
      case _ => false
    }
  }

}

case class Documentation(
  content: String
)

case class Argument(
  name: String,
  `type`: TypeStatement,
  docs: Seq[Documentation]
) extends Documented

sealed trait Parent extends TypeLike

sealed trait TypeLike extends Definition {
  def parents: Seq[Parent]
}

case class Type(
  parent: String,
  name: String,
  typeArguments: Seq[TypeParameter],
  parents: Seq[Parent],
  constructors: Seq[TypeConstructor],
  docs: Seq[Documentation]
) extends TypeLike with PackageLike with Documented {
  override def definitions: Seq[Definition] = constructors

  override def slice(interval: VersionsInterval): Type = {
    val filtered = constructors.filter { c =>
      c.versions.isIntersect(interval)
    }
    this.copy(constructors = filtered)
  }
}

case class ExternalType(
  parent: String,
  name: String,
  typeArguments: Seq[TypeParameter]
) extends Parent {
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
  parents: Seq[Parent],
  versions: VersionsInterval,
  docs: Seq[Documentation]
) extends TypeLike with TypeId with Documented with Versioned {
  override def packageName = parent
  val typeReference = TypeReference(parent)
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
  parents: Seq[Parent],
  httpRequest: Option[HttpRequest],
  versions: VersionsInterval,
  docs: Seq[Documentation]
) extends TypeLike with TypeId with Documented with Versioned

case class Trait(
  parent: String,
  name: String,
  parents: Seq[Trait],
  docs: Seq[Documentation]
) extends Parent with Documented

sealed trait PackageLike extends Definition {
  def name: String
  def definitions: Seq[Definition]

  def deepDefinitions: Seq[Definition] = {
    definitions ++ (definitions flatMap {
      case pl: PackageLike => pl.deepDefinitions
      case _ => Seq.empty
    })
  }

  def slice(interval: VersionsInterval): PackageLike

  protected def sliceInt(interval: VersionsInterval): Seq[Definition] = {
    definitions flatMap {
      case t: Type =>
        val sliced = t.slice(interval)
        if(sliced.constructors.nonEmpty) {
          Some(t)
        } else {
          None
        }
      case p: PackageLike =>
        Some(p.slice(interval))
      case c: Call =>
        if(c.versions.isIntersect(interval)) {
          Some(c)
        } else {
          None
        }
      case d: Definition =>
        Some(d)
    }
  }

}

case class Package(
  parent: String,
  name: String,
  definitions: Seq[Definition]
) extends PackageLike {
  override def slice(interval: VersionsInterval): Package = {
    this.copy(
      definitions = sliceInt(interval)
    )
  }
}

case class Schema(
  name: String,
  definitions: Seq[Definition]
) extends PackageLike {
  def parent = ""

  override def slice(interval: VersionsInterval): Schema = {
    this.copy(
      definitions = sliceInt(interval)
    )
  }

}
