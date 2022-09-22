package co.topl.codecs.json.tetra

import cats.implicits._
import co.topl.codecs.json.tetra.instances._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.models.{NetworkPrefix, SpendingAddress, TypedEvidence}
import io.circe.DecodingFailure
import io.circe.syntax._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scodec.bits.ByteVector

class DionAddressJsonCodecSpec extends AnyFlatSpec with Matchers {

  behavior of "Dion Address Json Codec"

  implicit val privateTestNetPrefix: NetworkPrefix = NetworkPrefix(64)

  it should "correctly encode the address 'AUAJSGFeLjJuE2ThYdXxXvRaJecRzPjmHzegvEgtoyURQ2zmUQav'" in {
    val expectedAddressString = "\"AUAJSGFeLjJuE2ThYdXxXvRaJecRzPjmHzegvEgtoyURQ2zmUQav\""

    val evidenceBytes =
      ByteVector.fromHex("0x0183ebe813408a46e2be7bd20e401944824690a741553710133124bf5d3855fb82").get

    val spendingAddress =
      SpendingAddress(TypedEvidence(evidenceBytes.head, Sized.strictUnsafe(evidenceBytes.tail)))

    val encodedAddress = spendingAddressEncoder(spendingAddress)

    encodedAddress.noSpaces shouldBe expectedAddressString
  }

  it should "correctly decode the address 'AUAJSGFeLjJuE2ThYdXxXvRaJecRzPjmHzegvEgtoyURQ2zmUQav'" in {
    val expectedEvidenceBytesString = "0183ebe813408a46e2be7bd20e401944824690a741553710133124bf5d3855fb82"

    val addressJson = "AUAJSGFeLjJuE2ThYdXxXvRaJecRzPjmHzegvEgtoyURQ2zmUQav"

    val decodedAddressResult =
      spendingAddressDecoder
        .decodeJson(addressJson.asJson)

    val decodedAddressValue = decodedAddressResult.valueOr(error => throw new Exception(error))

    val evidenceBytes = decodedAddressValue.typedEvidence.allBytes

    evidenceBytes.toHex shouldBe expectedEvidenceBytesString
  }

  it should "fail to decode the address if the network prefix doesn't match" in {

    val addressJson = "AUAJSGFeLjJuE2ThYdXxXvRaJecRzPjmHzegvEgtoyURQ2zmUQav"

    val decodedAddressResult =
      spendingAddressDecoder.decodeJson(addressJson.asJson)

    decodedAddressResult shouldBe Left(
      DecodingFailure("incorrect network prefix", Nil)
    )
  }

}
