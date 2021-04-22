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

  private val nodeGroupName = "TransactionTest"

  "A single node can handle transactions" in {
    val nodeConfig =
      ConfigFactory.parseString(
        raw"""bifrost.network.knownPeers = []
             |bifrost.rpcApi.namespaceSelector.debug = true
             |bifrost.forging.privateTestnet.genesisSeed = "$nodeGroupName"
             |""".stripMargin
      )
    val node: BifrostDockerNode =
      dockerSupport.createNode("bifrostTestNode", nodeGroupName)

    node.reconfigure(nodeConfig)

    node.start()

    node.waitForStartup().futureValue(Timeout(30.seconds)).value

    logger.info("Fetching addresses from Node")
    val addresses: List[Address] =
      node.run(ToplRpc.Admin.ListOpenKeyfiles.rpc)(ToplRpc.Admin.ListOpenKeyfiles.Params()).value.unlocked.toList

    logger.info("Creating raw poly transfer")
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

    logger.info("Broadcasting signed transaction")
    val broadcastedTx =
      node
        .run(ToplRpc.Transaction.BroadcastTx.rpc)(
          ToplRpc.Transaction.BroadcastTx.Params(signedTx)
        )
        .value

    broadcastedTx shouldEqual signedTx

    logger.info("Retrieving transaction from mempool")
    val memPoolTx =
      node.run(ToplRpc.NodeView.TransactionFromMempool.rpc)(ToplRpc.NodeView.TransactionFromMempool.Params(broadcastedTx.id)).value

    memPoolTx shouldEqual broadcastedTx

    val currentHeight =
      node.run(ToplRpc.NodeView.Head.rpc)(ToplRpc.NodeView.Head.Params()).value.height

    logger.info("Waiting 10 blocks for transaction to complete")
    node.pollUntilHeight(currentHeight + 10).futureValue(Timeout(60.seconds)).value

    logger.info("Checking for transaction in the chain")
    val ToplRpc.NodeView.TransactionById.Response(completedTransaction, _, _) =
      node.run(ToplRpc.NodeView.TransactionById.rpc)(ToplRpc.NodeView.TransactionById.Params(memPoolTx.id)).value

    completedTransaction shouldEqual memPoolTx
  }

  def genKeys(): Unit = keyRing.generateNewKeyPairs(10, Some(nodeGroupName))
  def clearKeyRing(): Unit = keyRing.addresses.map(keyRing.removeFromKeyring)

}
