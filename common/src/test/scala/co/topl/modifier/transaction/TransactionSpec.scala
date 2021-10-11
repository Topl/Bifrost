package co.topl.modifier.transaction

import cats.implicits._
import co.topl.attestation.AddressCodec.implicits._
import co.topl.attestation.Evidence.EvidenceContent
import co.topl.attestation.{Address, Evidence, PublicKeyPropositionCurve25519}
import co.topl.modifier.box._
import co.topl.modifier.transaction.builder.{BoxSelectionAlgorithms, TransferBuilder, TransferRequests}
import co.topl.modifier.{BoxReader, ModifierId}
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import co.topl.utils.codecs.implicits._
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class TransactionSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  behavior of "Transaction.id"

  /**
   * Implemented from Poly Transfer TX ID Test Vector
   * https://topl.atlassian.net/wiki/spaces/CORE/pages/359432273/Transaction+ID+Test+Vectors
   */
  it should "be expected value for poly transfer" in {
    val senderAddress =
      Base58Data.unsafe("AUAR7UA4Dz4j7hLnyax1KCK6eZH2aRiCqhTGNopmCVSPUV3NBQmT").decodeAddress.getOrThrow()
    val recipientAddress =
      Base58Data.unsafe("AUACbR67kDWBbCq3gXVpShwN8Z2jN5Q5Z4Vb1zwAV7ibXqgDr3z3").decodeAddress.getOrThrow()

    val boxState = mock[BoxReader[ProgramId, Address]]

    val polyBoxEvidenceData =
      Base58Data
        .unsafe("SDW2ANbYzKFQp6J2bQpU3n7gzeoS93kf1H9rqJgwzReW")
        .infalliblyEncodeAsBytes

    val polyBoxes =
      List(
        PolyBox(
          Evidence(
            polyBoxEvidenceData.head,
            EvidenceContent(polyBoxEvidenceData.tail)
          ),
          172705280970668827L,
          SimpleValue(2715996244082522697L)
        )
      ).some

    (boxState.getTokenBoxes _).expects(senderAddress).returns(polyBoxes)

    val fee = 100
    val sendAmount = 100000
    val data = Latin1Data.unsafe("test-poly-data").some

    val expectedModifierId = ModifierId.fromBase58(Base58Data.fromData(Array.fill(33)(1.toByte)))

    val polyTransfer =
      TransferBuilder
        .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](
          boxState,
          TransferRequests.PolyTransferRequest(
            List(senderAddress),
            List(recipientAddress -> sendAmount),
            senderAddress,
            fee,
            data
          ),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    polyTransfer.id shouldBe expectedModifierId
  }

  /**
   * Implemented from Arbit Transfer TX ID Test Vector
   * https://topl.atlassian.net/wiki/spaces/CORE/pages/359432273/Transaction+ID+Test+Vectors
   */
  it should "be expected value for arbit transfer" in {
    val senderAddress =
      Base58Data.unsafe("AUB4NNCWeyS6WXXmtDhbfmyPkzfJQVCLkEdqoPuufZ4eUckWUHYc").decodeAddress.getOrThrow()
    val recipientAddress =
      Base58Data.unsafe("AUDqwFTaYwCijcPRWtFFyBsk3Mkx8SbhKQGzY6MoUVXQVEgqC5pu").decodeAddress.getOrThrow()

    val arbitBoxNonce = 4484383924374665837L
    val arbitBoxEvidenceData = Base58Data.unsafe("zWk3bC3CCubnkwW1CfzgupN1SFRoBo3yb5MXLLhbMpZn").infalliblyEncodeAsBytes
    val arbitBoxValue = SimpleValue(393323367829459712L)

    val polyBoxNonce = 3623619089096119025L
    val polyBoxEvidenceData = Base58Data.unsafe("yV5WsbfKsVwu2syd4ZKb3HDNGck6WXQGRkD5vj8zgf17").infalliblyEncodeAsBytes
    val polyBoxValue = SimpleValue(1646574889335690014L)

    val boxes =
      List(
        ArbitBox(
          Evidence(arbitBoxEvidenceData.head, EvidenceContent(arbitBoxEvidenceData.tail)),
          arbitBoxNonce,
          arbitBoxValue
        ),
        PolyBox(
          Evidence(polyBoxEvidenceData.head, EvidenceContent(polyBoxEvidenceData.tail)),
          polyBoxNonce,
          polyBoxValue
        )
      ).some

    val boxState = mock[BoxReader[ProgramId, Address]]
    (boxState.getTokenBoxes _).expects(senderAddress).returns(boxes)

    val fee = 1000
    val sendAmount = 1400000
    val data = Latin1Data.unsafe("test-arbit-data").some

    val expectedModifierId = ModifierId.fromBase58(Base58Data.fromData(Array.fill(33)(1.toByte)))

    val arbitTransfer =
      TransferBuilder
        .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](
          boxState,
          TransferRequests.ArbitTransferRequest(
            List(senderAddress),
            List(recipientAddress -> sendAmount),
            senderAddress,
            senderAddress,
            fee,
            data
          ),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    arbitTransfer.id shouldBe expectedModifierId
  }

  /**
   * Implemented from Asset Transfer TX ID Test Vector
   * https://topl.atlassian.net/wiki/spaces/CORE/pages/359432273/Transaction+ID+Test+Vectors
   */
  it should "be expected value for asset transfer" in {
    val senderAddress =
      Base58Data.unsafe("AUAH1zvPVDc7N2zGG7iPq6QrE4vcES4Ub4G3rZD5DvAG3CqKCVmv").decodeAddress.getOrThrow()
    val recipientAddress =
      Base58Data.unsafe("AU9UNVXo2daiz5mwKizkv52kAXNMamk5FVS1wmbZJA6UgJzRRxu3").decodeAddress.getOrThrow()

    val assetBoxNonce = 7120796008434715411L
    val assetBoxEvidenceData = Base58Data.unsafe("NKz8DEfeuRELz6AKUouLSemmbDHPPt9aaveijw6xyC6m").infalliblyEncodeAsBytes
    val assetBoxIssuer =
      Base58Data.unsafe("AUDoKXJJtbUQNi6MJLH2jUjgQhzbd3HTuKPWAG69p2uUtpp59S9N").decodeAddress.getOrThrow()
    val assetShortName = Base58Data.unsafe("2L").infalliblyDecodeTo[Latin1Data]
    val assetCode = AssetCode(1.toByte, assetBoxIssuer, assetShortName)
    val assetBoxValue = AssetValue(6681012911373970621L, assetCode)

    val polyBoxNonce = 4861925802641168057L
    val polyBoxEvidenceData = Base58Data.unsafe("T9TWteRMNxv8Qjvh5B8qmFzzgDAUFXfF9HAw8XwBWAHP").infalliblyEncodeAsBytes
    val polyBoxValue = SimpleValue(390983730632400841L)

    val boxes =
      List(
        AssetBox(
          Evidence(assetBoxEvidenceData.head, EvidenceContent(assetBoxEvidenceData.tail)),
          assetBoxNonce,
          assetBoxValue
        ),
        PolyBox(
          Evidence(polyBoxEvidenceData.head, EvidenceContent(polyBoxEvidenceData.tail)),
          polyBoxNonce,
          polyBoxValue
        )
      ).some

    val boxState = mock[BoxReader[ProgramId, Address]]
    (boxState.getTokenBoxes _).expects(senderAddress).returns(boxes)

    val fee = 554
    val sendValue = AssetValue(1830000, assetCode)
    val data = Latin1Data.unsafe("test-asset-data").some
    val minting = false

    val expectedModifierId = ModifierId.fromBase58(Base58Data.fromData(Array.fill(33)(1.toByte)))

    val assetTransfer =
      TransferBuilder
        .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](
          boxState,
          TransferRequests.AssetTransferRequest(
            List(senderAddress),
            List(recipientAddress -> sendValue),
            senderAddress,
            senderAddress,
            fee,
            data,
            minting
          ),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    assetTransfer.id shouldBe expectedModifierId
  }
}
