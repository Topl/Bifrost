package co.topl.utils

import co.topl.crypto.hash.Digest
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import co.topl.utils.encode.Base58

object ScorexExtensions {
  object Digest32Ops {
    implicit val jsonEncoder: Encoder[Digest] = (d: Digest) => Base58.encode(d.toBytes).asJson
    implicit val jsonDecoder: Decoder[Digest] = Decoder.decodeString.map(str => Digest(Base58.decode(str).get))
  }
}
