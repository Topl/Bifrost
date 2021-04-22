package co.topl.it

import cats.data.NonEmptyChain
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.it.util._
import co.topl.rpc.ToplRpc
import com.typesafe.config.ConfigFactory
import org.scalatest.EitherValues
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class TransactionTest extends AnyFreeSpec with Matchers with IntegrationSuite with ScalaFutures with EitherValues {

  private val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519]()(
      networkPrefix,
      PrivateKeyCurve25519.secretGenerator,
      KeyfileCurve25519Companion
    )

  private val nodeGroupName = "test"

//  private val addresses =
//    List(
//      "AUAvJqLKc8Un3C6bC4aj8WgHZo74vamvX8Kdm6MhtdXgw51cGfix",
//      "AU9upSwu8MtmQz6EBMuv34bJ4G8i6Aw64xxRShJ3kpZRec5Ucp9Q",
//      "AU9NkZmX5Pch2kUA28GUtv9m4bNaLNtKLoFXphcAAc9PUQXinXRm",
//      "AU9avKWiVVPKyU9LoMqDpduS4knoLDMdPEK54qKDNBpdnAMwQZcS",
//      "AU9Xs4B5HnsTiYGb7D71CCxg5mYhaQv1WH3ptfiGbV4LUGb87W54",
//      "AUA3RmKwr39nVQFFTV1BQFELbFhJQVWfFDdS5YDx7r1om5UCbqef",
//      "AU9dn9YhqL1YWxfemMfS97zjVXR6G9QX74XRq1jVLtP3snQtuuVk",
//      "AUANVY6RqbJtTnQS1AFTQBjXMFYDknhV8NEixHFLmeZynMxVbp64",
//      "AU9sKKy7MN7U9G6QeasZUMTirD6SeGQx8Sngmb9jmDgNB2EzA3rq",
//      "AUAbSWQxzfoCN4FizrKKf6E1qCSRffHhjrvo2v7L6q8xFZ7pxKqh"
//    ).map(AddressEncoder.fromStringWithCheck(_, networkPrefix).value)

  "A single node can handle transactions" in {
    val nodeConfig =
      ConfigFactory.parseString(
        raw"""bifrost.network.knownPeers = []
             |bifrost.rpcApi.namespaceSelector.debug = true
             |""".stripMargin
      )
    val node: BifrostDockerNode =
      dockerSupport.createNode("bifrostTestNode", nodeGroupName)

    node.reconfigure(nodeConfig)

    node.start()

    node.waitForStartup().futureValue(Timeout(30.seconds)).value

    val addresses: List[Address] =
      node.run(ToplRpc.Admin.ListOpenKeyfiles.rpc)(ToplRpc.Admin.ListOpenKeyfiles.Params()).value.unlocked.toList

    val ToplRpc.Transaction.RawPolyTransfer.Response(rawTx, _) =
      node
        .run(ToplRpc.Transaction.RawPolyTransfer.rpc)(
          ToplRpc.Transaction.RawPolyTransfer.Params(
            propositionType = PublicKeyPropositionCurve25519.typeString,
            sender = NonEmptyChain.fromSeq(addresses).get,
            recipients = NonEmptyChain((addresses.head, 10)),
            fee = 0,
            changeAddress = addresses.head,
            data = None
          )
        )
        .value

    clearKeyRing()
    genKeys()

    val signedTx = rawTx.copy(attestation =
      keyRing.generateAttestation(addresses.toSet)(rawTx.messageToSign)
    )

    val broadcastedTx =
      node
        .run(ToplRpc.Transaction.BroadcastTx.rpc)(
          ToplRpc.Transaction.BroadcastTx.Params(signedTx)
        )
        .value

    broadcastedTx shouldEqual signedTx
  }

  def genKeys(): Unit = keyRing.generateNewKeyPairs(10, Some(nodeGroupName))
  def clearKeyRing(): Unit = keyRing.addresses.map(keyRing.removeFromKeyring)

}
