package bifrost.transaction.serialization

import bifrost.program.ProgramPreprocessor
import bifrost.program.{ExecutionBuilder, ExecutionBuilderTerms}
import bifrost.serialization.Serializer
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.parser.parse

import scala.util.Try

object ExecutionBuilderCompanion extends Serializer[ExecutionBuilder] {

  override def toBytes(a: ExecutionBuilder): Array[Byte] = {
    Bytes.concat(
      Longs.toByteArray(a.terms.json.noSpaces.getBytes.length),
      Longs.toByteArray(a.core.json.noSpaces.getBytes.length),
      Ints.toByteArray(a.assetCode.getBytes.length),
      a.assetCode.getBytes,
      a.terms.json.noSpaces.getBytes,
      a.core.json.noSpaces.getBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ExecutionBuilder] = Try {

    val Array(termsLength: Long, coreLength: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytes.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
    }.toArray

    var numBytesRead = 2 * Longs.BYTES

    val numStrBytes = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))

    numBytesRead += Ints.BYTES

    val assetCode: String = new String(bytes.slice(numBytesRead, numBytesRead + numStrBytes))

    numBytesRead += numStrBytes

    val terms: ExecutionBuilderTerms = parse(new String(
      bytes.slice(numBytesRead, numBytesRead + termsLength.toInt)
    )) match {
      case Left(_) => throw new Exception("ExecutionBuilderTerm json not properly formatted")
      case Right(x) => x.as[ExecutionBuilderTerms] match {
        case Left(_) => throw new Exception("ExecutionBuilder terms json was malformed")
        case Right(a: ExecutionBuilderTerms) => a
      }
    }

    numBytesRead += termsLength.toInt

    val core: ProgramPreprocessor = parse(new String(
      bytes.slice(numBytesRead, numBytesRead + coreLength.toInt)
    )) match {
      case Left(_) => throw new Exception("BaseModule json not properly formatted")
      case Right(x) => x.as[ProgramPreprocessor] match {
        case Left(_) => throw new Exception("Internal json was malformed in BaseModule")
        case Right(b: ProgramPreprocessor) => b
      }
    }

    ExecutionBuilder(terms, assetCode, core)
  }
}
