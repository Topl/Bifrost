package co.topl.utils.codecs.binary.modifier.transaction

import cats.{Eq, Show}
import co.topl.attestation.{Proposition, ThresholdPropositionCurve25519, ThresholdSignatureCurve25519}
import co.topl.modifier.box.SimpleValue
import co.topl.modifier.transaction.PolyTransfer
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.transaction.PolyTransferSerializer
import co.topl.utils.codecs.binary.modifier.codecs.polyTransferCodec

class PolyTransferCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[PolyTransfer[_ <: Proposition]] = Eq.fromUniversalEquals
  implicit private val show: Show[PolyTransfer[_ <: Proposition]] = Show.fromToString

  codecCompatabilityBehavior("poly transfer", polyTransferCodec, PolyTransferSerializer, polyTransferGen)

  "test" should "test" in {

    import cats.implicits._
    import co.topl.utils.codecs.binary.implicits._
    import co.topl.utils.StringDataTypes.implicits._
    import co.topl.utils.IdiomaticScalaTransition.implicits._

    forAll(addressGen, attestationThresholdCurve25519Gen) { (address, attestation) =>
      val polyTransfer = PolyTransfer(
        IndexedSeq(address -> 100),
        IndexedSeq(address -> SimpleValue(99)),
        attestation,
        9,
        6,
        None,
        false
      )

      val encodedResult = polyTransferCodec.encode(polyTransfer).map(_.toByteArray).map(_.encodeAsBase16).map(_.show)

      val encodedValue = encodedResult.getOrThrow()

      println(encodedValue)

      val bifrostEncodedResult = PolyTransferSerializer.toBytes(polyTransfer).encodeAsBase16.show

      println(bifrostEncodedResult)
    }
  }
}
