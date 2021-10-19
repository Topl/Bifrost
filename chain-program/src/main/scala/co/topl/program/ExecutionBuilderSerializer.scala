package co.topl.program

import co.topl.utils.codecs.binary.legacy._
import io.circe.parser
import io.circe.syntax.EncoderOps

object ExecutionBuilderSerializer extends BifrostSerializer[ExecutionBuilder] {

  override def serialize(obj: ExecutionBuilder, w: Writer): Unit = {
    w.putIntString(obj.terms.json.noSpaces)
    w.putIntString(obj.assetCode)
    w.putIntString(obj.core.asJson.noSpaces)
  }

  override def parse(r: Reader): ExecutionBuilder = {

    val terms: ExecutionBuilderTerms = parser.parse(r.getIntString()) match {
      case Left(_) => throw new Exception("ExecutionBuilderTerm json not properly formatted")
      case Right(x) =>
        x.as[ExecutionBuilderTerms] match {
          case Left(_)                         => throw new Exception("ExecutionBuilder terms json was malformed")
          case Right(a: ExecutionBuilderTerms) => a
        }
    }

    val assetCode: String = r.getIntString()

    val core: ProgramPreprocessor = parser.parse(r.getIntString()) match {
      case Left(_) => throw new Exception("BaseModule json not properly formatted")
      case Right(x) =>
        x.as[ProgramPreprocessor] match {
          case Left(_)                       => throw new Exception("Internal json was malformed in BaseModule")
          case Right(b: ProgramPreprocessor) => b
        }
    }

    ExecutionBuilder(terms, assetCode, core)
  }
}
