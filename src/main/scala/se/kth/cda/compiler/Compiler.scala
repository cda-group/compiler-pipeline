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
import se.kth.cda.compiler.dataflow.Metadata
import se.kth.cda.compiler.dataflow.optimize.OptimizeDFG._
import se.kth.cda.compiler.dataflow.enrich.EnrichDFG._

object Compiler {
  def compile(input: String): String = {

    val metadata = metadataDecoder(io.circe.parser.parse(input).getOrElse(Json.Null).hcursor) match {
    //val metadata = metadataDecoder.decodeJson(Json.fromString(input)) match {
      case Left(value) => throw value
      case Right(value) => value
    }

    val inputStream = CharStreams.fromString(metadata.arc_code)
    val lexer = new ArcLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    val arcparser = new ArcParser(tokenStream)
    val translator = Translator(arcparser)
    val ast = translator.expr()
    val expanded = MacroExpansion.expand(ast).get
    val typed = TypeInference.solve(expanded).get
    val dfg = typed.toDFG

    val enriched_dfg = dfg.enrich(metadata)
    val optimized_dfg = enriched_dfg.optimize

    //println(PrettyPrint.pretty(typed))
    //pprint.pprintln(optimized)

    encodeDFG(optimized_dfg).noSpaces
  }
}
