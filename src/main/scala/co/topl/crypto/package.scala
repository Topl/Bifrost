package co.topl

import io.circe.{Decoder, Encoder}
import io.circe.syntax.EncoderOps
import scorex.util.encode.Base58
import scorex.crypto.hash.Digest32

package object crypto {
  object Digest32Ops {
    implicit val jsonEncoder: Encoder[Digest32] = (d: Digest32) => Base58.encode(d).asJson
    implicit val jsonDecoder: Decoder[Digest32] = Decoder.decodeString.map(Base58.decode)
  }
}
