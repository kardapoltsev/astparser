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

import scala.io.Source
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._
import com.github.kardapoltsev.astparser.util.Logger



object TokenParsers {
  case class Identifier(name: String) extends Positional
  case class IntNumber(value: Int) extends Positional
}


private[astparser] trait TokenParsers extends Parsers {
  override type Elem = Token
  protected val IdentifierRegexp = "^[a-zA-Z][a-zA-Z0-9]*$".r
  protected val HexNumberRegexp = "^[0-9a-fA-F]+$".r
  protected val IntNumberRegexp = "^[-]?[0-9]+$".r

  import TokenParsers._
  import Tokens._

  val enableProfiling: Boolean = false

  private class Printer(val prefix: String = "### ", val indent: Int = 0) {
    def println(str: String): Unit = {
      val p = prefix + " " * indent
      Predef.println(str.lines.map(p + _).mkString(s"\n"))
    }

    def newIndent() = new Printer("", indent + 1)
  }

  private var printers: List[Printer] = List(new Printer())

  private def printer = printers.head

  private def println(str: String) = printer.println(str)

  private def printerIndent() = {
    printers = new Printer(printer.prefix, printer.indent + 1) :: printers
  }

  private def printerUnindent() = {
    printers = printers.tail
  }


  protected def profile[T](name: String)(p: Parser[T]) =
    if (enableProfiling)
      Parser { in =>
        println(s"{ $name --- start parsing")
        printerIndent()
        val start = System.currentTimeMillis
        val res = p(in)
        val end = System.currentTimeMillis
        val t = res match {
          case Success(x: Definition, _) => s"${x.getClass.getSimpleName}: name = ${x.name}"
          case Success(x: TypeConstructor, _) => s"${x.getClass.getSimpleName}: name = ${x.fullName}"
          case Success(x: Argument, _) => s"${x.getClass.getSimpleName}: $x"
          case Success(x: Identifier, _) => s"${x.getClass.getSimpleName}: $x"
          case Success(x: Reference, _) => s"${x.getClass.getSimpleName}: ${x.fullName}"
          case Success(x, _) => x.getClass.getSimpleName
          case x @ NoSuccess(m, _) => s"${x.getClass.getSimpleName}: $m"
        }
        printerUnindent()
        println(s"} $name => $t - parse in ${end - start}ms")
        res
      }
    else p



  protected def leftDoc = accept("prefix doc", {
    case ld @ LeftDoc(d) => Documentation(d)
  })

  protected def rightDoc = accept("suffix doc", {
    case rd @ RightDoc(d) => Documentation(d)
  })

  protected def identifierM =
    acceptIf {
      case Lexeme(x) =>
        x.nonEmpty &&
          (x.charAt(0).isLetter || x.charAt(0) == '_') &&
          !x.drop(1).exists(c => !c.isLetterOrDigit)
      case _ => false
    } { x => s"valid identifier expected but $x found" }

  protected def identifier: Parser[Identifier] = profile("identifier") {
    identifierM ^^ {
      case id: Lexeme => Identifier(id.chars).setPos(id.pos)
    }
  }

  protected def intNumber: Parser[IntNumber] = profile("intNumber") {
    accept("int number", {
      case l @ Lexeme(x) if IntNumberRegexp.unapplySeq(x).isDefined =>
        IntNumber(x.toInt).setPos(l.pos)
    })
  }

  protected def hexNumber: Parser[IntNumber] = profile("hexNumber") {
    accept("hex number", {
      case l @ Lexeme(x) if HexNumberRegexp.unapplySeq(x).isDefined =>
        //noinspection ScalaStyle
        IntNumber(java.lang.Long.parseLong(x, 16).toInt).setPos(l.pos)
    })
  }
}


class ParseException(message: String, val pos: Position, cause: Throwable = null) extends Exception(message, cause)


//noinspection ScalaStyle
class AstParser(override val enableProfiling: Boolean = false)
  extends TokenParsers with Parsers with Logger {
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
      path ^^ {
        names =>
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

  protected[astparser] def typeStatement: Parser[TypeStatement] = profile("typeStatement") {
    positioned {
      reference ~ opt(LeftBracket() ~> rep(typeStatement) <~ RightBracket()) ^^ {
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
      repLeftDoc ~ (TraitKeyword() ~> identifier) ~ typeExtensionExpr <~ opt(Semicolon()) ^^ {
        case ld ~ name ~ exts =>
          Trait(name.name, exts, ld)
      }
    }
  }

  protected def externalTypeDefinition: Parser[Definition] = profile("externalTypeDefinition") {
    positioned {
      (ExternalKeyword() ~> TypeKeyword()) ~ identifier ~ genericTypeParameters <~ opt(Semicolon()) ^^ {
        case kw ~ name ~ ta =>
          ExternalType(name.name, ta)
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
      (TypeKeyword() ~> identifier <~ Eq()) ~ (reference <~ opt(Semicolon())) ^^ {
        case name ~ ref =>
          TypeAlias(name.name, ref)
      }
    }
  }

  protected[astparser] def importDefinition: Parser[Import] = {
    positioned {
      (ImportKeyword() ~ reference <~ opt(Semicolon())) ^^ {
        case kw ~ reference =>
          Import(reference.name, reference)
      }
    }
  }

  protected def typeDefinition: Parser[Type] = {
    profile("typeDefinitionExpr") {
      ((repLeftDoc <~ TypeKeyword()) ~ identifier ~ genericTypeParameters ~ typeExtensionExpr ~ typeDefinitionBody) ^^ {
        case docs ~ name ~ gargs ~ parents ~ body =>
          Type(
            name.name,
            gargs,
            parents,
            body,
            docs
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
    opt(LeftBracket() ~> rep1(path) <~ RightBracket()) ^^ {
      args => args.getOrElse(Seq.empty).map { path =>
        TypeParameter(
          path.map(_.name).mkString("."),
          Seq.empty
        ).setPos(path.head.pos)
      }
    }
  }

  protected def typeExtensionExpr = {
    opt(extendsOperator ~> rep1(reference)) ^^ (_.getOrElse(Seq.empty))
  }

  protected def argumentsExpr = {
    opt(Eq() ~> rep1(argument)) ^^ (_.getOrElse(Seq.empty))
  }

  protected def typeConstructor = profile("typeConstructor") {
    positioned {
      repLeftDoc ~ opt(Dot()) ~ identifier ~ opt(hashId) ~ typeExtensionExpr ~ genericTypeParameters ~
          (argumentsExpr <~ opt(Semicolon())) ~ repRightDoc ^^ {
        case ld ~ dot ~ name ~ id ~ ext ~ ta ~ args ~ rd =>
          TypeConstructor(
            name.name,
            id.map(_.value),
            ta,
            args,
            ext,
            docs = ld ++ rd)
      }
    }
  }

  protected def callDefinition: Parser[Call] = profile("callDefinition") {
    positioned {
      repLeftDoc ~ CallKeyword() ~ (callDefinitionExp | failure("call definition expected")) ~ repRightDoc ^^ {
        case ld ~ kw ~ cd ~ rd => cd.copy(docs = ld ++ rd)
      }
    }
  }

  protected def callDefinitionExp: Parser[Call] = {
    identifier ~ opt(hashId) ~ typeExtensionExpr ~ argumentsExpr ~
        (responseOperator ~> typeStatement <~ opt(Semicolon())) ^^ {
      case name ~ id ~ parents ~ args ~ rtype =>
        Call(
          name.name,
          id.map(_.value),
          args,
          rtype,
          parents = parents,
          docs = Seq.empty
        )
    }
  }

  protected def apackage: Parser[Package] = profile("package") {
    positioned {
      (repLeftDoc <~ PackageKeyword()) ~ reference ~ (LeftBrace() ~> definitions <~ RightBrace()) ^^ {
        case ld ~ name ~ ds =>
          packagesFromString(name.fullName, ds, doc = ld, position = name.pos)
      }
    }
  }


  def schema: Parser[Schema] = profile("schema") {
    positioned {
      phrase(schemaInfoExp ~ definitions) ^^ {
        case sn ~ maybeVersion ~ ds =>
          val v = SchemaVersion(
            maybeVersion.map(_.value).getOrElse(1), ds
          ).setPos(sn.pos)
          Schema(sn.name, Seq(v))
      }
    }
  }


  def schemaInfoExp = profile("schemaInfo") {
    (SchemaKeyword() ~> identifier <~ opt(Semicolon())) ~
      opt(VersionKeyword() ~> intNumber <~ opt(Semicolon()))
  }


  def definitions: Parser[List[Definition]] = {
    profile("definitions") {
      rep(
        definition
          | failure("definition expected")
      )
    }
  }

  private def packagesFromString(
    fullName: String,
    definitions: Seq[Definition],
    doc: Seq[Documentation] = Seq.empty,
    position: Position
  ): Package = {
    val names = fullName.split("\\.").toSeq.reverse
    val inner = Package(names.head, definitions).setPos(position)
    names.tail.foldLeft(inner) {
      case (acc, n) =>
        Package(n, Seq(acc)).setPos(position)
    }
  }

  protected def tryParse[T](
    parser: Parser[T], input: CharSequence, sourceName: String
  ): ParseResult[T] = {
    parser(new lexer.Scanner(
      new ReaderWithSourcePosition(new CharSequenceReader(input), sourceName)
    ))
  }


  def tryParse(input: CharSequence, sourceName: String): ParseResult[Schema] = {
    val reader = new ReaderWithSourcePosition(new CharSequenceReader(input), sourceName)
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

  private def getResult[T](res: ParseResult[T]): T = res match {
    case Success(v, _) => v
    case NoSuccess(msg, next) => throw new ParseException(s"$msg; at `${next.first}`:${next.pos}", next.pos)
  }
}
