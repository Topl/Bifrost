package co.topl.codecs.binary.scodecs.genesisBlob

import co.topl.codecs.binary.scodecs.attestation.keyManagement.privateKeyCurve25519Codec
import co.topl.codecs.binary.scodecs.modifier.blockCodec
import co.topl.codecs.binary.scodecs.valuetypes._
import co.topl.modifier.block.GenesisBlob
import scodec.Codec

trait GenesisBlobCodecs {

  implicit val genesisBlobCodec: Codec[GenesisBlob] =
    (blockCodec :: seqCodec(privateKeyCurve25519Codec)).as[GenesisBlob]
}
