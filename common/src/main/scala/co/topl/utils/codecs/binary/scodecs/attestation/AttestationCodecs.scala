package co.topl.utils.codecs.binary.scodecs.attestation

import co.topl.attestation.{Address, Evidence}
import co.topl.utils.codecs.binary.scodecs.valuetypes._
import scodec.Codec
import shapeless.{::, HList, HNil}

trait AttestationCodecs
    extends keyManagement.KeyManagementCodecs
    with proof.ProofCodecs
    with proposition.PropositionCodecs {
  implicit val evidenceCodec: Codec[Evidence] = bytesCodec(Evidence.size).as[Evidence]

  implicit val addressCodec: Codec[Address] =
    (byteCodec :: evidenceCodec)
      .xmapc[Address] { case byte :: evidence :: HNil => Address(evidence)(byte) }(address =>
        HList(address.networkPrefix, address.evidence)
      )
}
