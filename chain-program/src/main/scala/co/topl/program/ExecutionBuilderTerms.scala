package co.topl.program

import cats.implicits._
import co.topl.utils.Gzip
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.codecs.implicits._
import io.circe.syntax._
import io.circe.{Decoder, Json}

case class ExecutionBuilderTerms(terms: String) {
  /*  */
  require(terms.length < 16 * 1024)

  lazy val json: Json = if (terms.length > 1024) {
    s"gzip:${Gzip.compress(terms.getBytes).encodeAsBase58}".asJson
  } else {
    terms.asJson
  }

  override def toString: String = s"ExecutionBuilderTerms(${json.toString})"
}

object ExecutionBuilderTerms {

  sealed trait DecodeGzipFailure
  case class InvalidZippedData(message: String) extends DecodeGzipFailure

  def decodeBase58GzipData(zippedData: Base58Data): String = {
    val zipped: Array[Byte] = zippedData.value
    val unzipped: Array[Byte] = Gzip.decompress(zipped)
    new String(unzipped)
  }

  def decodeGzip(zippedData: String): Either[DecodeGzipFailure, String] =
    Base58Data.validated(zippedData).map(decodeBase58GzipData).leftMap(err => InvalidZippedData(err.toString)).toEither

  implicit val decodeTerms: Decoder[ExecutionBuilderTerms] =
    Decoder[String]
      // note: using either type here as a path split instead of error handling, both left and right are valid
      .map { terms =>
        if (terms.startsWith("gzip:")) terms.substring("gzip:".length).asRight[ExecutionBuilderTerms]
        else ExecutionBuilderTerms(terms).asLeft[String]
      }
      .emap {
        case Right(terms) => decodeGzip(terms).map(ExecutionBuilderTerms(_)).leftMap(_.toString)
        case Left(terms)  => terms.asRight[String]
      }
}
