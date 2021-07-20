package co.topl.rpc

import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.modifier.box._
import co.topl.utils.NetworkType
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import io.circe.syntax._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ToplRpcCodecsSpec extends AnyFlatSpec with ToplRpcCodecs with Matchers with EitherValues {

  implicit private val networkPrefix: NetworkPrefix = NetworkType.PrivateTestnet.netPrefix

  private val address = PrivateKeyCurve25519.secretGenerator.generateSecret("test".getBytes)._2.address
  private val assetCode = AssetCode(1: Byte, address, Latin1Data.unsafe("test"))

  behavior of "ToplRpcCodecs"

  it should "encode and decode EntryBoxes with values for each box type" in {
    val boxes = ToplRpc.NodeView.Balances.EntryBoxes(
      List(PolyBox(address.evidence, 0L, SimpleValue(50))),
      List(ArbitBox(address.evidence, 0L, SimpleValue(50))),
      List(AssetBox(address.evidence, 0L, AssetValue(50, assetCode)))
    )

    val encoded = boxes.asJson

    val decoded = encoded.as[ToplRpc.NodeView.Balances.EntryBoxes].value

    boxes shouldBe decoded
  }

  it should "decode EntryBoxes with a missing box type" in {
    val polyBox = PolyBox(address.evidence, 0L, SimpleValue(50))
    val raw = raw"""{"PolyBox": [${polyBox.asJson}]}"""
    val parsedPolyBox = io.circe.parser.parse(raw).flatMap(_.as[ToplRpc.NodeView.Balances.EntryBoxes]).value
    parsedPolyBox shouldBe ToplRpc.NodeView.Balances.EntryBoxes(List(polyBox), Nil, Nil)
  }

}
