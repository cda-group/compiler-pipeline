package se.kth.cda.compiler

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import se.kth.cda.arc.syntaxtree.PrettyPrint
import se.kth.cda.arc.syntaxtree.parser.Translator
import se.kth.cda.arc.syntaxtree.transformer.MacroExpansion
import se.kth.cda.arc.syntaxtree.typer.TypeInference
import se.kth.cda.arc.{ArcLexer, ArcParser}
import se.kth.cda.compiler.dataflow.transform.ToDFG._

object Compiler {
  def compile(code: String): String = {

    val inputStream = CharStreams.fromString(code)
    val lexer = new ArcLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser = new ArcParser(tokenStream)
    val translator = Translator(parser)
    val ast = translator.expr()
    val expanded = MacroExpansion.expand(ast).get
    val typed = TypeInference.solve(expanded).get
    val dfg = typed.toDFG

    println(PrettyPrint.pretty(typed))
    pprint.pprintln(dfg)
    ""

    //dfg.asJson.noSpaces
  }
}
