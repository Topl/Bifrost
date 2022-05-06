package co.topl.codecs.binary.scodecs.genesisAndKeys

import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.codecs.binary.scodecs.attestation.keyManagement.privateKeyCurve25519Codec
import co.topl.codecs.binary.scodecs.modifier.blockCodec
import co.topl.codecs.binary.scodecs.valuetypes._
import co.topl.modifier.block.Block
import scodec.Codec
import shapeless.{::, HList, HNil}

trait GenesisAndKeysCodecs {

  implicit val genesisAndKeysCodecs: Codec[GenesisAndKeys] =
    (blockCodec :: seqCodec(privateKeyCurve25519Codec)).xmapc[GenesisAndKeys] { case genesis :: keys :: HNil =>
      GenesisAndKeys(genesis, keys)
    }(genesisAndKeys => HList(genesisAndKeys.genesis, genesisAndKeys.keys))
}

case class GenesisAndKeys(genesis: Block, keys: Seq[PrivateKeyCurve25519])
