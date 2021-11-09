package co.topl.modifier.transaction

import cats.implicits._
import co.topl.attestation.AddressCodec.implicits._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.ModifierId
import co.topl.modifier.box._
import co.topl.utils.{CommonGenerators, EqMatcher}
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.codecs._
import co.topl.utils.catsInstances._

import scala.collection.immutable.ListMap

class TransactionSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues
    with EqMatcher {

  behavior of "Transaction.id"

  /**
   * Implemented from Poly Transfer TX ID Test Vector
   * https://topl.atlassian.net/wiki/spaces/CORE/pages/359432273/Transaction+ID+Test+Vectors
   */
  it should "be expected value for poly transfer" in {
    val senderAddress = asAddress("AUAR7UA4Dz4j7hLnyax1KCK6eZH2aRiCqhTGNopmCVSPUV3NBQmT")

    val recipientAddress = asAddress("AUACbR67kDWBbCq3gXVpShwN8Z2jN5Q5Z4Vb1zwAV7ibXqgDr3z3")

    val polyBoxNonce = 172705280970668827L

    val timestamp = 1633978713303L

    val fee = 100

    val polyTransferAmount = 100000

    val data = "test-poly-data"

    val minting = false

    val expectedModifierId = asModifierId("qzs41EUoabyGw2HDQyK3KYm4vBCYbVEcfe8grWYVnceX")

    val polyTransfer =
      PolyTransfer[PublicKeyPropositionCurve25519](
        IndexedSeq(senderAddress    -> polyBoxNonce),
        IndexedSeq(recipientAddress -> SimpleValue(polyTransferAmount)),
        ListMap(),
        timestamp,
        fee,
        Latin1Data.unsafe(data).some,
        minting
      )

    polyTransfer.id should eqvShow(expectedModifierId)
  }

  /**
   * Implemented from Arbit Transfer TX ID Test Vector
   * https://topl.atlassian.net/wiki/spaces/CORE/pages/359432273/Transaction+ID+Test+Vectors
   */
  it should "be expected value for arbit transfer" in {
    val senderAddress = asAddress("AUB4NNCWeyS6WXXmtDhbfmyPkzfJQVCLkEdqoPuufZ4eUckWUHYc")

    val recipientAddress = asAddress("AUDqwFTaYwCijcPRWtFFyBsk3Mkx8SbhKQGzY6MoUVXQVEgqC5pu")

    val arbitBoxNonce = 4484383924374665837L

    val timestamp = 1633979011442L

    val fee = 1000

    val arbitTransferAmount = 1400000

    val data = "test-arbit-data"

    val minting = false

    val expectedModifierId = asModifierId("bVohDkizmv1HD2uyEBF5xh7M4oNyX9rEz5YZ8qavFNsP")

    val arbitTransfer =
      ArbitTransfer[PublicKeyPropositionCurve25519](
        IndexedSeq(senderAddress    -> arbitBoxNonce),
        IndexedSeq(recipientAddress -> SimpleValue(arbitTransferAmount)),
        ListMap(),
        timestamp,
        fee,
        Latin1Data.unsafe(data).some,
        minting
      )

    arbitTransfer.id should eqvShow(expectedModifierId)
  }

  /**
   * Implemented from Asset Transfer TX ID Test Vector
   * https://topl.atlassian.net/wiki/spaces/CORE/pages/359432273/Transaction+ID+Test+Vectors
   */
  it should "be expected value for asset transfer" in {
    val senderAddress = asAddress("AUAH1zvPVDc7N2zGG7iPq6QrE4vcES4Ub4G3rZD5DvAG3CqKCVmv")

    val recipientAddress = asAddress("AU9UNVXo2daiz5mwKizkv52kAXNMamk5FVS1wmbZJA6UgJzRRxu3")

    val assetBoxNonce = 7120796008434715411L

    val assetCodeIssuer = asAddress("AUDoKXJJtbUQNi6MJLH2jUjgQhzbd3HTuKPWAG69p2uUtpp59S9N")

    val assetCodeShortName = "2L"

    val assetCodeVersion = 1.toByte

    val timestamp = 1633979336451L

    val fee = 554

    val assetTransferAmount = 1830000

    val data = "test-asset-data"

    val minting = false

    val expectedModifierId = asModifierId("rijPy1ABXbtUmUpm2WfmM48RBnkkH5gyKPLUTrgFEKhQ")

    val assetTransfer =
      AssetTransfer[PublicKeyPropositionCurve25519](
        IndexedSeq(senderAddress -> assetBoxNonce),
        IndexedSeq(
          recipientAddress -> AssetValue(
            assetTransferAmount,
            AssetCode(assetCodeVersion, assetCodeIssuer, Latin1Data.unsafe(assetCodeShortName))
          )
        ),
        ListMap(),
        timestamp,
        fee,
        Latin1Data.unsafe(data).some,
        minting
      )

    assetTransfer.id should eqvShow(expectedModifierId)
  }

  def asAddress(addressString: String): Address =
    Base58Data.unsafe(addressString).decodeAddress.getOrThrow()

  def asModifierId(modifierIdString: String): ModifierId =
    Base58Data.unsafe(modifierIdString).decodeTransmitted[ModifierId].getOrThrow()
}
