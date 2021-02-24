package co.topl.utils

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base58

object ScorexExtensions {
  object Digest32Ops {
    implicit val jsonEncoder: Encoder[Digest32] = (d: Digest32) => Base58.encode(d).asJson
    implicit val jsonDecoder: Decoder[Digest32] = Decoder.decodeString.map(str => Digest32 @@ Base58.decode(str).get)
  }
}
