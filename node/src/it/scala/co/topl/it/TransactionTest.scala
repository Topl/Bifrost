package co.topl.it

import cats.data.NonEmptyChain
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}

import co.topl.it.util._
import co.topl.modifier.transaction.Transaction
import co.topl.attestation._
import co.topl.rpc.ToplRpc
import co.topl.utils.Int128
import com.typesafe.config.ConfigFactory
import org.scalatest._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration._
import scorex.crypto.hash.Blake2b256

/**
  * 1. Start a node from genesis
  * 2. Extract the 10 keys from genesis.  Also generate the same 10 keys in a local keyring using the same seed.
  * 3. Send polys from address "A" to address "B".  Verify balance change.  Verify no change for address "C".
  * 4. Generate a new key in the local keyring.  (Address "D")
  * 5. Send polys from address "A" to address "D".  Verify balance change.
  * 6. Send polys from address "D" to a burn address (doesn't exist in any keyring).  Verify balance change.
  */
class TransactionTest
    extends AnyFreeSpec
    with Matchers
    with IntegrationSuite
    with ScalaFutures
    with EitherValues
    with TryValues {

  private val nodeGroupName = "TransactionTest"

  private val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519]()(
      networkPrefix,
      PrivateKeyCurve25519.secretGenerator,
      KeyfileCurve25519Companion
    )

  keyRing.generateNewKeyPairs(10, Some(nodeGroupName))

  private val nodeConfig =
    ConfigFactory.parseString(
      raw"""bifrost.network.knownPeers = []
           |bifrost.rpcApi.namespaceSelector.debug = true
           |bifrost.forging.privateTestnet.genesisSeed = "$nodeGroupName"
           |""".stripMargin
    )

  private val burnAddress = {
    val bytes: Array[Byte] = Array(networkPrefix, PublicKeyPropositionCurve25519.typePrefix) ++ Array.fill(32)(2: Byte)
    val checksum = Blake2b256(bytes).take(AddressEncoder.checksumLength)
    AddressEncoder.validateAddress(bytes ++ checksum, networkPrefix).value
  }

  "A single node can handle transactions" in {
    val node: BifrostDockerNode =
      dockerSupport.createNode("bifrostTestNode", nodeGroupName)

    node.reconfigure(nodeConfig)

    node.start()

    node.waitForStartup().futureValue(Timeout(30.seconds)).value

    logger.info("Fetching addresses from Node")
    val addresses @ addressA :: addressB :: addressC :: _: List[Address] =
      node.run(ToplRpc.Admin.ListOpenKeyfiles.rpc)(ToplRpc.Admin.ListOpenKeyfiles.Params()).value.unlocked.toList

    keyRing.addresses shouldBe addresses.toSet

    sendAndAwaitTransaction(node)(
      name = "tx1",
      sender = NonEmptyChain(addressA),
      recipients = NonEmptyChain((addressB, 10)),
      changeAddress = addressA
    )

    balanceFor(node)(addressA).Balances.Polys shouldBe Int128(999990)
    balanceFor(node)(addressB).Balances.Polys shouldBe Int128(1000010)
    balanceFor(node)(addressC).Balances.Polys shouldBe Int128(1000000)

    val addressD = keyRing.generateNewKeyPairs().success.value.head

    sendAndAwaitTransaction(node)(
      name = "tx2",
      sender = NonEmptyChain(addressA),
      recipients = NonEmptyChain((addressD.publicImage.address, 10)),
      changeAddress = addressA
    )

    balanceFor(node)(addressA).Balances.Polys shouldBe Int128(999980)
    balanceFor(node)(addressD.publicImage.address).Balances.Polys shouldBe Int128(10)

    sendAndAwaitTransaction(node)(
      name = "tx3",
      sender = NonEmptyChain(addressD.publicImage.address),
      recipients = NonEmptyChain((burnAddress, 10)),
      changeAddress = addressA
    )

    balanceFor(node)(burnAddress).Balances.Polys shouldBe Int128(10)
    balanceFor(node)(addressD.publicImage.address).Balances.Polys shouldBe Int128(0)
  }

  private def awaitBlocks(node: BifrostDockerNode)(count: Int): Unit = {
    val currentHeight =
      node.run(ToplRpc.NodeView.Head.rpc)(ToplRpc.NodeView.Head.Params()).value.height

    logger.info(s"Waiting $count blocks")
    node.pollUntilHeight(currentHeight + count).futureValue(Timeout((count * 4).seconds)).value
  }

  private def sendAndAwaitTransaction(node: BifrostDockerNode)(
    name:                                   String,
    sender:                                 NonEmptyChain[Address],
    recipients:                             NonEmptyChain[(Address, Int128)],
    changeAddress:                          Address
  ): Transaction.TX = {
    logger.info(s"Creating $name")
    val ToplRpc.Transaction.RawPolyTransfer.Response(rawTx, _) =
      node
        .run(ToplRpc.Transaction.RawPolyTransfer.rpc)(
          ToplRpc.Transaction.RawPolyTransfer.Params(
            propositionType = PublicKeyPropositionCurve25519.typeString,
            sender = sender,
            recipients = recipients,
            fee = 0,
            changeAddress = changeAddress,
            data = None
          )
        )
        .value

    val signedTx1 = rawTx.copy(attestation = keyRing.generateAttestation(sender.iterator.toSet)(rawTx.messageToSign))

    logger.info(s"Broadcasting signed $name")
    val broadcastedTx =
      node
        .run(ToplRpc.Transaction.BroadcastTx.rpc)(
          ToplRpc.Transaction.BroadcastTx.Params(signedTx1)
        )
        .value

    broadcastedTx shouldEqual signedTx1

    logger.info(s"Retrieving $name from mempool")
    val memPoolTx =
      node
        .run(ToplRpc.NodeView.TransactionFromMempool.rpc)(
          ToplRpc.NodeView.TransactionFromMempool.Params(broadcastedTx.id)
        )
        .value

    memPoolTx shouldEqual broadcastedTx

    awaitBlocks(node)(10)

    logger.info(s"Checking for $name in the chain")
    val ToplRpc.NodeView.TransactionById.Response(completedTransaction, _, _) =
      node.run(ToplRpc.NodeView.TransactionById.rpc)(ToplRpc.NodeView.TransactionById.Params(memPoolTx.id)).value

    completedTransaction shouldEqual memPoolTx

    logger.info(s"$name complete: $completedTransaction")

    completedTransaction
  }

  private def balanceFor(node: BifrostDockerNode)(address: Address): ToplRpc.NodeView.Balances.Entry =
    node.run(ToplRpc.NodeView.Balances.rpc)(ToplRpc.NodeView.Balances.Params(List(address))).value(address)

}
