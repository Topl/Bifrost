package co.topl.program

import co.topl.utils.Gzip
import co.topl.utils.encode.Base58
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import co.topl.utils.codecs.AsBytes.implicits._
import co.topl.utils.StringTypes.Base58String

case class ExecutionBuilderTerms(terms: String) {
  /*  */
  require(terms.length < 16 * 1024)

  lazy val json: Json = if (terms.length > 1024) {
    s"gzip:${Base58.encode(Gzip.compress(terms.getBytes))}".asJson
  } else {
    terms.asJson
  }

  override def toString: String = s"ExecutionBuilderTerms(${json.toString})"
}

object ExecutionBuilderTerms {

  def decodeGzip(zippedStr: Base58String): String = {
    val zipped: Array[Byte] = Base58.decode(zippedStr)
    val unzipped: Array[Byte] = Gzip.decompress(zipped)
    new String(unzipped)
  }

  implicit val decodeTerms: Decoder[ExecutionBuilderTerms] = (c: HCursor) =>
    for {
      terms <- c.as[String]
    } yield
      if (terms.startsWith("gzip:")) {
        Base58String
          .validated(terms.substring("gzip:".length))
          .map(decodeGzip)
          .map(ExecutionBuilderTerms(_))
          .getOrElse(ExecutionBuilderTerms(""))
      } else {
        ExecutionBuilderTerms(terms)
      }
}
