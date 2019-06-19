package se.kth.cda.compiler

import org.scalatest._

import org.antlr.v4.runtime.{ CharStreams, CommonTokenStream }
import org.scalatest.{ Assertion, FunSuite, Matchers }
import se.kth.cda.arc.syntaxtree.{ AST, PrettyPrint }
import se.kth.cda.arc.syntaxtree.parser.Translator
import se.kth.cda.arc.syntaxtree.transformer.MacroExpansion
import se.kth.cda.arc.syntaxtree.typer.TypeInference
import se.kth.cda.arc.{ ArcLexer, ArcParser }

import scala.language.implicitConversions

class ParserTests extends FunSuite with Matchers {

  test("printy") {
    val testS = """
    |in: stream[i32], out: streamappender[i32]|
      for(in, out, |b, _, i| merge(b, let x = 5; i+x))
""";
    val typed = compile(testS);
    println(PrettyPrint.pretty(typed));
    val statements = AST.ArcStatements(List(typed));
    println(statements);
    val dfg = ASTDataFlowConverter.transform(statements);
    println(dfg)
  }

  private def compile(input: String): AST.Expr = {
    val inputStream = CharStreams.fromString(input);
    val lexer = new ArcLexer(inputStream);
    val tokenStream = new CommonTokenStream(lexer);
    val parser = new ArcParser(tokenStream);
    val translator = Translator(parser);
    val ast = translator.expr();
    val expanded = MacroExpansion.expand(ast).get;
    val typed = TypeInference.solve(expanded).get;
    typed
  }
}
