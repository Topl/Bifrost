package co.topl.codecs.json.genesisBlob

import cats.implicits._
import co.topl.codecs.binary._
import co.topl.codecs.json.valuetypes._
import co.topl.modifier.block.GenesisBlob
import co.topl.utils.StringDataTypes.Base58Data
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import scodec.Codec
import scodec.bits.BitVector

trait GenesisBlobJsonCodecs {

  implicit val genesisBlobJsonEncoder: Encoder[GenesisBlob] = { genesisBlob =>
    Codec[GenesisBlob]
      .encode(genesisBlob)
      .getOrElse(throw new Exception("Unable to encode genesis block and keys"))
      .encodeAsBase58
      .asJson
  }

  implicit val genesisBlobJsonDecoder: Decoder[GenesisBlob] =
    Decoder[Base58Data].emap { bytes =>
      Codec[GenesisBlob]
        .decode(BitVector(bytes.value))
        .toEither
        .map(_.value)
        .leftMap(_.toString)
    }
}
