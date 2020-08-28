package bifrost.program

import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.parser

import scala.util.Try

object ExecutionBuilderCompanion extends BifrostSerializer[ExecutionBuilder] {

  override def serialize(obj: ExecutionBuilder, w: Writer): Unit = {
    w.putIntString(obj.terms.json.noSpaces)
    w.putIntString(obj.assetCode)
    w.putIntString(obj.core.json.noSpaces)
  }

  override def parse(r: Reader): ExecutionBuilder = {

    val terms: ExecutionBuilderTerms = parser.parse(r.getIntString()) match {
      case Left(_) => throw new Exception("ExecutionBuilderTerm json not properly formatted")
      case Right(x) => x.as[ExecutionBuilderTerms] match {
        case Left(_) => throw new Exception("ExecutionBuilder terms json was malformed")
        case Right(a: ExecutionBuilderTerms) => a
      }
    }

    val assetCode: String = r.getIntString()

    val core: ProgramPreprocessor = parser.parse(r.getIntString()) match {
      case Left(_) => throw new Exception("BaseModule json not properly formatted")
      case Right(x) => x.as[ProgramPreprocessor] match {
        case Left(_) => throw new Exception("Internal json was malformed in BaseModule")
        case Right(b: ProgramPreprocessor) => b
      }
    }

    ExecutionBuilder(terms, assetCode, core)
  }

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

    val terms: ExecutionBuilderTerms = parser.parse(new String(
      bytes.slice(numBytesRead, numBytesRead + termsLength.toInt)
    )) match {
      case Left(_) => throw new Exception("ExecutionBuilderTerm json not properly formatted")
      case Right(x) => x.as[ExecutionBuilderTerms] match {
        case Left(_) => throw new Exception("ExecutionBuilder terms json was malformed")
        case Right(a: ExecutionBuilderTerms) => a
      }
    }

    numBytesRead += termsLength.toInt

    val core: ProgramPreprocessor = parser.parse(new String(
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
