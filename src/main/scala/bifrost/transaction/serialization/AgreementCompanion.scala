package bifrost.transaction.serialization

import bifrost.contract.ProgramPreprocessor
import bifrost.contract.{Agreement, AgreementTerms}
import bifrost.serialization.Serializer
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.parser.parse

import scala.util.Try

object AgreementCompanion extends Serializer[Agreement] {

  override def toBytes(a: Agreement): Array[Byte] = {
    Bytes.concat(
      Longs.toByteArray(a.terms.json.noSpaces.getBytes.length),
      Longs.toByteArray(a.core.json.noSpaces.getBytes.length),
      Ints.toByteArray(a.assetCode.getBytes.length),
      a.assetCode.getBytes,
      a.terms.json.noSpaces.getBytes,
      a.core.json.noSpaces.getBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[Agreement] = Try {

    val Array(termsLength: Long, coreLength: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytes.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
    }.toArray

    var numBytesRead = 2 * Longs.BYTES

    val numStrBytes = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))

    numBytesRead += Ints.BYTES

    val assetCode: String = new String(bytes.slice(numBytesRead, numBytesRead + numStrBytes))

    numBytesRead += numStrBytes

    val terms: AgreementTerms = parse(new String(
      bytes.slice(numBytesRead, numBytesRead + termsLength.toInt)
    )) match {
      case Left(_) => throw new Exception("AgreementTerm json not properly formatted")
      case Right(x) => x.as[AgreementTerms] match {
        case Left(_) => throw new Exception("Agreement terms json was malformed")
        case Right(a: AgreementTerms) => a
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

    Agreement(terms, assetCode, core)
  }
}
