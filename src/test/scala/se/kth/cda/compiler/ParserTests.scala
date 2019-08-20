package se.kth.cda.compiler

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.scalatest.{FunSuite, Matchers}
import se.kth.cda.arc.syntaxtree.AST
import se.kth.cda.arc.syntaxtree.parser.Translator
import se.kth.cda.arc.syntaxtree.typer.TypeInference
import se.kth.cda.arc.{ArcLexer, ArcParser}

import scala.language.implicitConversions

class ParserTests extends FunSuite with Matchers {

  private def compile(input: String): AST.Expr = {
    val inputStream = CharStreams.fromString(input)
    val lexer = new ArcLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser = new ArcParser(tokenStream)
    val translator = Translator(parser)
    val ast = translator.expr()
    //val expanded = MacroExpansion.expand(ast).get
    val typed = TypeInference.solve(ast).get
    typed
  }

  test("normalize") {
    val input =
      """
      |{
      |  "nodes": [
      |    {
      |      "id": "source_0",
      |      "kind": {
      |        "Source": {
      |          "format": "CSV",
      |          "kind": {
      |            "LocalFile": {
      |              "path": "input.txt"
      |            }
      |          }
      |        }
      |      }
      |    },
      |    {
      |      "id": "sink_0",
      |      "kind": {
      |        "Sink": {
      |          "format": "CSV",
      |          "kind": {
      |            "LocalFile": {
      |              "path": "output.txt"
      |            }
      |          }
      |        }
      |      }
      |    }
      |  ],
      |  "timestamp_extractor": 0,
      |  "arc_code": "|source_0: stream[i64], sink_0: streamappender[?]|\nlet operator_1 = result(for(source_0, windower[unit,appender[?],?,vec[?]](\n  |ts,windows,state| { [ts/60L], () },\n  \t|wm,windows,state| { result(for(windows, appender, |b,i,e| if(i < wm, merge(b, i), b))), () },\n  \t|agg| result(agg)\n), |sb,si,se| merge(sb, se)));\nfor(operator_1, sink_0, |sb,si,se| merge(sb,\nlet obj102 = (se);\nlet obj105 = (result(\n    for(\n        obj102,\n        merger[i64, +],\n        |b: merger[i64, +], i: i64, e: i64| \n            merge(b, e)\n    )\n));\nlet obj106 = (len(obj102));\nlet obj107 = (obj105 / obj106);\nlet obj108 = (result(\n    for(obj102, \n        appender[i64], \n        |b: appender[i64], i: i64, e: i64| \n            merge(b, e / obj107)\n    )\n));\nobj108\n))\n"
      |}
      """.stripMargin

    println(input)
    val output = Compiler.compile(input)
    println(output)
  }
}
