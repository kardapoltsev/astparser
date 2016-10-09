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
  def idHex: String = f"$id%08x"
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

trait Parent extends TypeLike

trait TypeLike extends Definition {
  def parents: Seq[Parent]
}

case class Type(
  parent: String,
  name: String,
  typeArguments: Seq[TypeParameter],
  parents: Seq[Parent],
  constructors: Seq[TypeConstructor],
  docs: Seq[Documentation]
) extends TypeLike with Documented

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
  parents: Seq[Parent],
  docs: Seq[Documentation]
) extends TypeLike with TypeId with Documented

case class Trait(
  parent: String,
  name: String,
  parents: Seq[Trait],
  docs: Seq[Documentation]
) extends Parent with Documented

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





