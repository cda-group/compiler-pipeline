package se.kth.cda.compiler

import io.circe.Json
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import se.kth.cda.arc.syntaxtree.parser.Translator
import se.kth.cda.arc.syntaxtree.typer.TypeInference
import se.kth.cda.arc.{ArcLexer, ArcParser}
import se.kth.cda.compiler.dataflow.IdGenerator
import se.kth.cda.compiler.dataflow.decode.DecodeMetadata.metadataDecoder
import se.kth.cda.compiler.dataflow.deploy.Deploy._
import se.kth.cda.compiler.dataflow.encode.EncodeDFG.encodeDFG
import se.kth.cda.compiler.dataflow.enrich.EnrichDFG._
import se.kth.cda.compiler.dataflow.optimize.OptimizeDFG._
import se.kth.cda.compiler.dataflow.transform.ToDFG._

object Compiler {
  def compile(input: String): String = {

    val metadata = metadataDecoder(io.circe.parser.parse(input).getOrElse(Json.Null).hcursor) match {
      case Left(value)  => throw value
      case Right(value) => value
    }
    //print(metadata)

    IdGenerator.resetGlobal()

    val inputStream = CharStreams.fromString(metadata.arc_code)
    val lexer = new ArcLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    val arcparser = new ArcParser(tokenStream)
    val translator = Translator(arcparser)
    val ast = translator.expr()
    //val expanded = MacroExpansion.expand(ast).get
    val typed = TypeInference.solve(ast).get
    val dfg = typed.toDFG
    //import se.kth.cda.arc.syntaxtree.PrettyPrint._
    //println(pretty(typed))
    //println(dfg.pretty)

    val enriched_dfg = dfg.enrich(metadata)
    val optimized_dfg = enriched_dfg.optimize(fusion = true)
    //println(optimized_dfg.pretty)
    //println(enriched_dfg.pretty)

    //println(PrettyPrint.pretty(typed))
    //pprint.pprintln(optimized)

    val ordered_dfg = optimized_dfg.order
    encodeDFG(ordered_dfg).noSpaces
  }
}
