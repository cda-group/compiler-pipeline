package se.kth.cda.compiler

import io.circe.Json
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import se.kth.cda.arc.syntaxtree.PrettyPrint
import se.kth.cda.arc.syntaxtree.parser.Translator
import se.kth.cda.arc.syntaxtree.transformer.MacroExpansion
import se.kth.cda.arc.syntaxtree.typer.TypeInference
import se.kth.cda.arc.{ArcLexer, ArcParser}
import se.kth.cda.compiler.dataflow.transform.ToDFG._
import se.kth.cda.compiler.dataflow.encode.EncodeDFG.encodeDFG
import se.kth.cda.compiler.dataflow.decode.DecodeMetadata.metadataDecoder
import se.kth.cda.compiler.dataflow.{IdGenerator, Metadata}
import se.kth.cda.compiler.dataflow.optimize.OptimizeDFG._
import se.kth.cda.compiler.dataflow.enrich.EnrichDFG._
import se.kth.cda.compiler.dataflow.pretty.PrettyPrint._

object Compiler {
  def compile(input: String): String = {

    val metadata = metadataDecoder(io.circe.parser.parse(input).getOrElse(Json.Null).hcursor) match {
      case Left(value) => throw value
      case Right(value) => value
    }

    IdGenerator.reset()

    val inputStream = CharStreams.fromString(metadata.arc_code)
    val lexer = new ArcLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    val arcparser = new ArcParser(tokenStream)
    val translator = Translator(arcparser)
    val ast = translator.expr()
    val expanded = MacroExpansion.expand(ast).get
    val typed = TypeInference.solve(expanded).get
    val dfg = typed.toDFG
    //println(dfg.pretty)

    //val optimized_dfg = dfg.optimize
    val enriched_dfg = dfg.enrich(metadata)
    println(enriched_dfg.pretty)

    //println(PrettyPrint.pretty(typed))
    //pprint.pprintln(optimized)

    encodeDFG(enriched_dfg).noSpaces
  }
}
