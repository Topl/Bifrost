package co.topl.utils.codecs

import co.topl.utils.Int128
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

object Int128Codec {
  implicit val jsonEncoder: Encoder[Int128] = (x: Int128) => x.toString.asJson
  implicit val jsonDecoder: Decoder[Int128] = Decoder.decodeString.map(s => Int128(s))
}
