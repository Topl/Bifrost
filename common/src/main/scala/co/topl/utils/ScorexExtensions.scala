package co.topl.utils

import cats.implicits._
import co.topl.crypto.hash.Digest32
import co.topl.utils.AsBytes.implicits._
import co.topl.utils.StringTypes.Base58String
import co.topl.utils.encode.Base58
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

object ScorexExtensions {

  object Digest32Ops {

    implicit val jsonEncoder: Encoder[Digest32] = (d: Digest32) =>
      Base58.encode(d).map(_.value.value).getOrElse("").asJson

    implicit val jsonDecoder: Decoder[Digest32] =
      Decoder.decodeString
        .emap { str =>
          for {
            base58String  <- Base58String.validated(str).leftMap(_ => "Value is not a valid Base58")
            decodedBase58 <- Base58.decode(base58String).leftMap(_ => "Value did not decode with Base58")
            digest32      <- Digest32.validated(decodedBase58).leftMap(_ => "Value is not a valid digest 32")
          } yield digest32
        }
  }
}
