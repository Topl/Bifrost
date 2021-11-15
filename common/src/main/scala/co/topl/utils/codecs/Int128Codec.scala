package co.topl.utils.codecs

import co.topl.utils.Int128
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

object Int128Codec {

  trait JsonInstances {
    implicit val int128JsonEncoder: Encoder[Int128] = (x: Int128) => x.toString.asJson
    implicit val int128JsonDecoder: Decoder[Int128] = Decoder.decodeString.map(s => Int128(s))
  }

  trait Implicits extends JsonInstances

  object implicits extends Implicits
}
