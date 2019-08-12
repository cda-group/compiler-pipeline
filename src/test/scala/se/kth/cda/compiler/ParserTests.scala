package se.kth.cda.compiler

import io.circe.syntax._
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.scalatest.{FunSuite, Matchers}
import se.kth.cda.arc.syntaxtree.parser.Translator
import se.kth.cda.arc.syntaxtree.transformer.MacroExpansion
import se.kth.cda.arc.syntaxtree.typer.TypeInference
import se.kth.cda.arc.syntaxtree.{AST, PrettyPrint}
import se.kth.cda.arc.{ArcLexer, ArcParser}
import se.kth.cda.compiler.dataflow.JsonEncoder._
import se.kth.cda.compiler.dataflow.optimize.OptimizeDFG._

import scala.language.implicitConversions

class ParserTests extends FunSuite with Matchers {

  test("printy") {
    import se.kth.cda.compiler.dataflow.transform.ToDFG.ToDFG
    val typedCode =
      """
      #|in: stream[i32], out: streamappender[i32]|
      # let mapper = result(for(in, streamappender[i32], |b, _, e| merge(b, e + 5)));
      # for(mapper, out, |b,_,e| merge(b, e - 5))
      """.stripMargin('#')
    val _ =
      """
      #|in: stream[i32], out: streamappender[i32]|
      # let mapper = result(for(in, streamappender[i32], |b, _, e| merge(b, e + 5)));
      # let tumbler = result(for(mapper,
      #  windower[unit, merger[i32,+], i32, i32](
      #   |ts, windows, state| { [ ts / 60L ] , () },
      #   |wm, windows, state| { filter(windows, |ts| ts < wm) , () },
      #   |agg| result(agg)
      #  ),
      #  |w, _, e| merge(w, e)
      # ));
      # let filterer = result(for(tumbler, streamappender[i32], |b, _, e| if(e > 20, b, merge(b, e)));
      # for(filterer, out, |b,_,e| merge(b, e - 5))
      """.stripMargin('#')
    val typed = compile(typedCode)
    //println(PrettyPrint.pretty(typed))
    val dfg = typed.toDFG
    dfg.optimize()
    //pprint.pprintln(dfg, height = 200)
    //pprint.pprintln(dfg, height = 200)
    println(dfg.asJson)
  }

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
}
