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

import com.github.kardapoltsev.astparser.parser.TokenParsers.{Identifier, IntNumber}

import scala.io.Source
import scala.util.parsing.input._

object TokenParsers {
  case class Identifier(name: String) extends Positional
  case class IntNumber(value: Int)    extends Positional
}

class ParseException(message: String, val pos: Position) extends Exception(message)

class AstParser(
  override protected val enableProfiling: Boolean = false
) extends BaseParser {

  override type Elem = Token

  protected val IdentifierRegexp = "^[a-zA-Z][a-zA-Z0-9]*$".r
  protected val HexNumberRegexp  = "^[0-9a-fA-F]+$".r
  protected val IntNumberRegexp  = "^[-]?[0-9]+$".r

  import com.github.kardapoltsev.astparser.parser.Tokens._

  protected val lexer = new Lexer

  protected def repLeftDoc = profile("repLeftDoc") {
    rep(leftDoc)
  }

  protected def repRightDoc = profile("repRightDoc") {
    rep(rightDoc)
  }

  protected[astparser] def reference = profile("reference") {
    positioned {
      path ^^ { names =>
        //log.debug(s"parsed ref names: $names at ${names.head.pos}")
        Reference(
          names.map(_.name).mkString(".")
        )
      }
    }
  }

  private def path = profile("name") {
    rep1sep(identifier, Dot())
  }

  private def extendsOperator = LessSign() ~ Colon()

  private def responseOperator = Eq() ~ GreaterSign()

  private def argumentsOperator = Colon() ~ Colon()

  protected[astparser] def typeStatement: Parser[TypeStatement] = profile("typeStatement") {
    positioned {
      reference ~ opt(LeftBracket() ~> rep1sep(typeStatement, Comma()) <~ RightBracket()) ^^ {
        case ref ~ ts =>
          TypeStatement(
            ref,
            ts.getOrElse(Seq.empty)
          )
      }
    }
  }

  protected def argument: Parser[Argument] = profile("argument") {
    positioned {
      repLeftDoc ~ identifier ~ (Colon() ~> typeStatement) ~ repRightDoc ^^ {
        case ld ~ name ~ ts ~ rd =>
          Argument(name.name, ts, ld ++ rd)
      }
    }
  }

  protected def hashId = Hash() ~> hexNumber

  protected def traitDefinition: Parser[Trait] = {
    positioned {
      repLeftDoc ~ constraint ~ (TraitKeyword() ~> identifier) ~ typeExtensionExpr ~ argumentsExpr ^^ {
        case ld ~ c ~ name ~ exts ~ arguments =>
          Trait(name.name, arguments, exts, ld, c)
      }
    }
  }

  protected def externalTypeDefinition: Parser[Definition] = profile("externalTypeDefinition") {
    positioned {
      constraint ~ (ExternalKeyword() ~> TypeKeyword()) ~ identifier ~ genericTypeParameters ^^ {
        case c ~ kw ~ name ~ ta =>
          ExternalType(name.name, ta, c)
      }
    }
  }

  protected def definition: Parser[Definition] = {
    profile("definition") {
      positioned {
        externalTypeDefinition |
          typeAlias |
          typeDefinition |
          externalTypeDefinition |
          traitDefinition |
          importDefinition |
          callDefinition |
          apackage
      }
    }
  }

  protected def typeAlias: Parser[TypeAlias] = {
    profile("typeAliasExp") {
      constraint ~ (TypeKeyword() ~> identifier <~ Eq()) ~ typeStatement ^^ {
        case c ~ name ~ aType =>
          TypeAlias(name.name, aType, c)
      }
    }
  }

  protected[astparser] def importDefinition: Parser[Import] = {
    positioned {
      constraint ~ (ImportKeyword() ~> reference) ^^ {
        case c ~ reference =>
          Import(reference.name, reference, c)
      }
    }
  }

  protected def typeDefinition: Parser[Type] = {
    profile("typeDefinitionExpr") {
      repLeftDoc ~ constraint ~ (TypeKeyword() ~> identifier) ~ genericTypeParameters ~ typeExtensionExpr ~ typeDefinitionBody ^^ {
        case docs ~ c ~ name ~ gargs ~ parents ~ body =>
          Type(
            name.name,
            gargs,
            parents,
            body,
            docs,
            c
          )
      }
    }
  }

  protected def typeDefinitionBody: Parser[Seq[TypeConstructor]] = {
    profile("typeDefinitionBody") {
      LeftBrace() ~> rep1(typeConstructor) <~ RightBrace()
    }
  }

  protected def genericTypeParameters: Parser[Seq[TypeParameter]] = {
    opt(LeftBracket() ~> rep1sep(path, Comma()) <~ RightBracket()) ^^ { args =>
      args.getOrElse(Seq.empty).map { path =>
        TypeParameter(
          path.map(_.name).mkString("."),
          Seq.empty
        ).setPos(path.head.pos)
      }
    }
  }

  protected def typeExtensionExpr: Parser[Seq[Reference]] = {
    opt(extendsOperator ~> rep1(reference)) ^^ { exts =>
      exts.getOrElse(Seq.empty)
    }
  }

  protected def modernArgumentsExpr = {
    argumentsOperator ~> rep(argument)
  }

  protected def argumentsExpr = {
    opt(modernArgumentsExpr) ^^ (_.getOrElse(Seq.empty))
  }

  protected def typeConstructor = profile("typeConstructor") {
    positioned {
      repLeftDoc ~ constraint ~ opt(Dot()) ~ identifier ~ opt(hashId) ~ opt(versionsInterval) ~ typeExtensionExpr ~
        genericTypeParameters ~ argumentsExpr ~ repRightDoc ^^ {
        case ld ~ c ~ _ ~ name ~ id ~ int ~ ext ~ ta ~ args ~ rd =>
          val i = int.getOrElse(VersionsInterval(None, None))
          TypeConstructor(name.name, id.map(_.value), ta, args, ext, i, docs = ld ++ rd, c)
      }
    }
  }

  protected def versionsInterval: Parser[VersionsInterval] = profile("versions interval") {
    positioned {
      (LeftParen() ~>
        opt(intNumber)) ~ (opt(Dash()) ~> opt(intNumber)
        <~ RightParen()) ^^ {
        case maybeStart ~ maybeEnd =>
          VersionsInterval(maybeStart.map(_.value), maybeEnd.map(_.value))
      }
    }
  }

  protected def callDefinition: Parser[Call] = profile("callDefinition") {
    positioned {
      repLeftDoc ~ constraint ~ opt(httpDefinition) ~ (CallKeyword() ~> identifier) ~ opt(hashId) ~ opt(
        versionsInterval
      ) ~ typeExtensionExpr ~ argumentsExpr ~
        (responseOperator ~> typeStatement) ^^ {
        case ld ~ c ~ httpDef ~ name ~ id ~ int ~ parents ~ args ~ rtype =>
          val i = int.getOrElse(VersionsInterval(None, None))
          Call(
            name.name,
            id.map(_.value),
            args,
            rtype,
            parents = parents,
            httpRequest = httpDef,
            i,
            docs = ld,
            constraint = c
          )
      }
    }
  }

  private def httpDefinition: Parser[String] = {
    accept(
      "httpDefinition",
      {
        case Http(content) => content
      }
    )
  }

  protected def apackage: Parser[Package] = profile("package") {
    positioned {
      repLeftDoc ~ constraint ~ (PackageKeyword() ~> reference) ~ (LeftBrace() ~> definitions <~ RightBrace()) ^^ {
        case ld ~ c ~ name ~ ds =>
          packagesFromString(name.fullName, ds, doc = ld, position = name.pos, c)
      }
    }
  }

  def schema: Parser[Schema] = profile("schema") {
    positioned {
      phrase(constraint ~ schemaInfoExp ~ definitions) ^^ {
        case c ~ sn ~ ds =>
          Schema(sn.name, ds, c)
      }
    }
  }

  def schemaInfoExp = profile("schemaInfo") {
    SchemaKeyword() ~> identifier
  }

  def definitions: Parser[List[Definition]] = {
    profile("definitions") {
      rep(
        definition
          | failure("definition expected")
      )
    }
  }

  protected def leftDoc = positioned {
    accept(
      "prefix doc",
      {
        case LeftDoc(d) => Documentation(d)
      }
    )
  }

  protected def rightDoc = positioned {
    accept(
      "suffix doc",
      {
        case RightDoc(d) => Documentation(d)
      }
    )
  }

  protected def identifierM: Parser[Elem] =
    acceptIf {
      case Lexeme(x) =>
        x.nonEmpty &&
          (x.charAt(0).isLetter || x.charAt(0) == '_') &&
          !x.drop(1).exists(c => !c.isLetterOrDigit)
      case _ => false
    } { x =>
      s"valid identifier expected but $x found"
    }

  protected def identifier: Parser[Identifier] = profile("identifier") {
    identifierM ^^ {
      case id: Lexeme => Identifier(id.chars).setPos(id.pos)
      case _          => throw new IllegalArgumentException("Only Lexeme expected")
    }
  }

  protected def intNumber: Parser[IntNumber] = profile("intNumber") {
    accept(
      "int number",
      {
        case l @ Lexeme(x) if IntNumberRegexp.unapplySeq(x).isDefined =>
          IntNumber(x.toInt).setPos(l.pos)
      }
    )
  }

  protected def hexNumber: Parser[IntNumber] = profile("hexNumber") {
    accept(
      "hex number",
      {
        case l @ Lexeme(x) if HexNumberRegexp.unapplySeq(x).isDefined =>
          //noinspection ScalaStyle
          IntNumber(java.lang.Long.parseLong(x, 16).toInt).setPos(l.pos)
      }
    )
  }

  protected def constraint: Parser[Constraint] = profile("constraints") {
    positioned {
      rep(maybeEnableConstraint | maybeDisableConstraint) ^^ { constraints =>
        val enable = constraints.collect {
          case c: EnableConstraint => c.constraints
        }.flatten.distinct
        val disable = constraints.collect {
          case c: DisableConstraint => c.constraints
        }.flatten.distinct
        Constraint(EnableConstraint(enable), DisableConstraint(disable))
      }
    }
  }

  private def maybeEnableConstraint: Parser[EnableConstraint] = profile("enableConstraint") {
    positioned {
      QuestionMark() ~> rep1sep(identifier, Comma()) ^^ { constraints =>
        EnableConstraint(constraints.map(_.name))
      }
    }
  }

  private def maybeDisableConstraint: Parser[DisableConstraint] = profile("disableConstraint") {
    positioned {
      QuestionMark() ~> ExclamationMark() ~> rep1sep(identifier, Comma()) ^^ { constraints =>
        DisableConstraint(constraints.map(_.name))
      }
    }
  }

  private def packagesFromString(
    fullName: String,
    definitions: Seq[Definition],
    doc: Seq[Documentation],
    position: Position,
    constraint: Constraint
  ): Package = {
    val names = fullName.split("\\.").toSeq.reverse
    val inner = Package(names.head, definitions, constraint).setPos(position)
    names.tail.foldLeft(inner) {
      case (acc, n) =>
        Package(n, Seq(acc), constraint).setPos(position)
    }
  }

  protected def tryParse[T](
    parser: Parser[T],
    input: CharSequence,
    sourceName: String
  ): ParseResult[T] = {
    parser(
      new lexer.Scanner(
        new ReaderWithSourcePosition(new CharSequenceReader(input), sourceName)
      )
    )
  }

  def tryParse(input: CharSequence, sourceName: String): ParseResult[Schema] = {
    val reader  = new ReaderWithSourcePosition(new CharSequenceReader(input), sourceName)
    val scanner = new lexer.Scanner(reader)
    schema(scanner)
  }

  protected def parse[T](parser: Parser[T], input: CharSequence, sourceName: String): T = {
    getResult(tryParse(parser, input, sourceName))
  }

  def parse(input: CharSequence, sourceName: String): Schema = loggingTime(s"parse $sourceName") {
    getResult(tryParse(input, sourceName))
  }

  def parse(file: java.io.File): Schema = {
    val content = Source.fromFile(file).getLines().mkString(System.lineSeparator())
    parse(content, file.getAbsolutePath)
  }

}
