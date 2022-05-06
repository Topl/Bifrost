package co.topl.codecs.json.genesisAndKeys

import cats.implicits._
import co.topl.codecs.base58JsonEncoder
import co.topl.codecs.binary._
import co.topl.codecs.binary.scodecs.genesisAndKeys.GenesisAndKeys
import co.topl.codecs.json.valuetypes.base58JsonDecoder
import co.topl.utils.StringDataTypes.Base58Data
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import scodec.Codec
import scodec.bits.BitVector

trait GenesisAndKeysJsonCodecs {

  implicit val genesisAndKeysJsonEncoder: Encoder[GenesisAndKeys] = { genesisAndKeys =>
    Codec[GenesisAndKeys]
      .encode(genesisAndKeys)
      .getOrElse(throw new Exception("Unable to encode genesis block and keys"))
      .encodeAsBase58
      .asJson
  }

  implicit val genesisAndKeysJsonDecoder: Decoder[GenesisAndKeys] =
    Decoder[Base58Data].emap { bytes =>
      Codec[GenesisAndKeys]
        .decode(BitVector(bytes.value))
        .toEither
        .map(_.value)
        .leftMap(_.toString)
    }
}
