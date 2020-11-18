package co.topl.program

import co.topl.utils.Gzip
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import scorex.util.encode.Base64

case class ExecutionBuilderTerms(terms: String){
  /*  */
  require(terms.length < 16*1024)
  lazy val json: Json = if(terms.length > 1024) {
    s"gzip:${Base64.encode(Gzip.compress(terms.getBytes))}".asJson
  } else {
    terms.asJson
  }

  override def toString: String = s"ExecutionBuilderTerms(${json.toString})"
}

object ExecutionBuilderTerms {

    def decodeGzip(zippedStr: String): String = {
      val zipped: Array[Byte] = Base64.decode(zippedStr).get
      val unzipped: Array[Byte] = Gzip.decompress(zipped)
      new String(unzipped)
    }

  implicit val decodeTerms: Decoder[ExecutionBuilderTerms] = (c: HCursor) => for {
    terms <- c.as[String]
  } yield {
    if (terms.startsWith("gzip:")) {
      ExecutionBuilderTerms(decodeGzip(terms.substring("gzip:".length)))
    } else {
      ExecutionBuilderTerms(terms)
    }
  }
}