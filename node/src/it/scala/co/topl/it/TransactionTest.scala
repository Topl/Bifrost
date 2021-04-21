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

    val addresses: List[Address] =
      node.run(ToplRpc.Admin.ListOpenKeyfiles.rpc)(ToplRpc.Admin.ListOpenKeyfiles.Params()).value.unlocked.toList

    val ToplRpc.Transaction.RawPolyTransfer.Response(rawTx, messageToSign) =
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
