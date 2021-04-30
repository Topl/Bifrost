package co.topl.utils

import co.topl.crypto.hash.Digest32
import co.topl.utils.BytesOf.Implicits._
import co.topl.utils.encode.Base58
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

object ScorexExtensions {

  object Digest32Ops {
    implicit val jsonEncoder: Encoder[Digest32] = (d: Digest32) => Base58.encode(d).asJson
    implicit val jsonDecoder: Decoder[Digest32] = Decoder.decodeString.map(str => Digest32(Base58.decode(str).get))
  }
}
