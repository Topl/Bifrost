package co.topl.utils.codecs.binary

import co.topl.attestation.{Address, Evidence}
import co.topl.utils.codecs.binary.valuetypes.codecs._
import scodec.Codec
import shapeless.{::, HList, HNil}

package object attestation {

  trait Codecs extends proof.Codecs with proposition.Codecs {

    implicit val evidenceCodec: Codec[Evidence] = bytesCodec(Evidence.size).as[Evidence]

    implicit val addressCodec: Codec[Address] =
      (byteCodec :: evidenceCodec)
        .xmapc[Address] { case byte :: evidence :: HNil => Address(evidence)(byte) }(address =>
          HList(address.networkPrefix, address.evidence)
        )
  }

  object codecs extends Codecs
}
