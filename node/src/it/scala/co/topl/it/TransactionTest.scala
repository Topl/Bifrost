package co.topl.it

import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.attestation.{Address, AddressEncoder, PublicKeyPropositionCurve25519}
import co.topl.it.util._
import co.topl.modifier.box.AssetCode
import co.topl.rpc.ToplRpc
import com.typesafe.config.ConfigFactory
import org.scalatest.EitherValues
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class TransactionTest extends AnyFreeSpec with Matchers with IntegrationSuite with ScalaFutures with EitherValues {

  val externalAddress: NonEmptyChain[Address] = NonEmptyChain(
    "AUAvJqLKc8Un3C6bC4aj8WgHZo74vamvX8Kdm6MhtdXgw51cGfix",
    "AU9upSwu8MtmQz6EBMuv34bJ4G8i6Aw64xxRShJ3kpZRec5Ucp9Q",
    "AU9NkZmX5Pch2kUA28GUtv9m4bNaLNtKLoFXphcAAc9PUQXinXRm",
    "AU9avKWiVVPKyU9LoMqDpduS4knoLDMdPEK54qKDNBpdnAMwQZcS",
    "AU9Xs4B5HnsTiYGb7D71CCxg5mYhaQv1WH3ptfiGbV4LUGb87W54",
    "AUA3RmKwr39nVQFFTV1BQFELbFhJQVWfFDdS5YDx7r1om5UCbqef",
    "AU9dn9YhqL1YWxfemMfS97zjVXR6G9QX74XRq1jVLtP3snQtuuVk",
    "AUANVY6RqbJtTnQS1AFTQBjXMFYDknhV8NEixHFLmeZynMxVbp64",
    "AU9sKKy7MN7U9G6QeasZUMTirD6SeGQx8Sngmb9jmDgNB2EzA3rq",
    "AUAbSWQxzfoCN4FizrKKf6E1qCSRffHhjrvo2v7L6q8xFZ7pxKqh"
  ).map(s => AddressEncoder.fromStringWithCheck(s, networkPrefix).value)

  val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519]()(
      networkPrefix,
      PrivateKeyCurve25519.secretGenerator,
      KeyfileCurve25519Companion
    )

  val assetCode: AssetCode = AssetCode(1: Byte, externalAddress.head, "test_1")

  "A single node can handle transactions" in {
    val nodeConfig =
      ConfigFactory.parseString(
        raw"""bifrost.network.knownPeers = []
             |bifrost.rpcApi.namespaceSelector.debug = true
             |""".stripMargin
      )
    val node: BifrostDockerNode =
      dockerSupport.createNode("bifrostTestNode", "TransactionTest")

    node.reconfigure(nodeConfig)

    node.start()

    node.waitForStartup().futureValue(Timeout(30.seconds)).value

    val ToplRpc.Transaction.RawPolyTransfer.Response(rawTx, messageToSign) =
      node
        .run(ToplRpc.Transaction.RawPolyTransfer.rpc)(
          ToplRpc.Transaction.RawPolyTransfer.Params(
            propositionType = PublicKeyPropositionCurve25519.typeString,
            sender = externalAddress,
            recipients = NonEmptyChain((externalAddress.head, 10)),
            fee = 0,
            changeAddress = externalAddress.head,
            data = None
          )
        )
        .value

    val signedTx = rawTx.copy(attestation =
      keyRing.addresses.map(keyRing.generateAttestation(_)(rawTx.messageToSign)).reduce(_ ++ _)
    )

    val broadcastedTx =
      node
        .run(ToplRpc.Transaction.BroadcastTx.rpc)(
          ToplRpc.Transaction.BroadcastTx.Params(signedTx)
        )
        .value

    broadcastedTx shouldEqual signedTx
  }

}
