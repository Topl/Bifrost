package co.topl.codecs.json.crypto

import cats.implicits._
import co.topl.codecs.json.valuetypes._
import co.topl.crypto.hash.digest.{Digest32, Digest64}
import co.topl.utils.StringDataTypes.Base58Data
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

trait CryptoCodecs {
  implicit val digest32JsonEncoder: Encoder[Digest32] = (d: Digest32) => Base58Data.fromData(d.value).asJson

  implicit val digest32JsonDecoder: Decoder[Digest32] =
    Decoder[Base58Data]
      .emap(data => Digest32.validated(data.value).toEither.leftMap(err => s"failed to decode 32-byte digest: $err"))

  implicit val digest64JsonEncoder: Encoder[Digest64] =
    digest => Base58Data.fromData(digest.value).asJson

  implicit val digest64JsonDecoder: Decoder[Digest64] =
    Decoder[Base58Data].emap(data =>
      Digest64.validated(data.value).leftMap(err => s"failed to decode 64-byte digest: $err").toEither
    )
}
