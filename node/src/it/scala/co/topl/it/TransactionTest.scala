package co.topl.it

import cats.data.NonEmptyChain
import co.topl.attestation._
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.it.util._
import co.topl.modifier.box.{AssetBox, AssetCode, AssetValue}
import co.topl.modifier.transaction.Transaction
import co.topl.rpc.ToplRpc
import co.topl.utils.Int128
import com.typesafe.config.ConfigFactory
import org.scalatest._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.hash.Blake2b256

import scala.concurrent.duration._

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

  private var node: BifrostDockerNode = _
  private var addresses: List[Address] = _
  private def addressA: Address = addresses.head
  private def addressB: Address = addresses(1)
  private def addressC: Address = addresses(2)
  private var keyD: PrivateKeyCurve25519 = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    node = dockerSupport.createNode("bifrostTestNode", nodeGroupName)
    node.reconfigure(nodeConfig)
    node.start()
    node.waitForStartup().futureValue(Timeout(30.seconds)).value

    logger.info("Fetching addresses from Node")
    addresses =
      node.run(ToplRpc.Admin.ListOpenKeyfiles.rpc)(ToplRpc.Admin.ListOpenKeyfiles.Params()).value.unlocked.toList
    keyRing.addresses shouldBe addresses.toSet

    keyD = keyRing.generateNewKeyPairs().success.value.head
  }

  "Polys can be sent from address A to addressB" in {
    sendAndAwaitPolyTransaction(
      name = "tx-poly-a-to-b-10",
      sender = NonEmptyChain(addressA),
      recipients = NonEmptyChain((addressB, 10)),
      changeAddress = addressA
    )

    balancesFor(addressA).Balances.Polys shouldBe Int128(999990)
    balancesFor(addressB).Balances.Polys shouldBe Int128(1000010)
    balancesFor(addressC).Balances.Polys shouldBe Int128(1000000)
  }

  "Polys can be sent from addressA to addressD" in {
    sendAndAwaitPolyTransaction(
      name = "tx-poly-a-to-d-10",
      sender = NonEmptyChain(addressA),
      recipients = NonEmptyChain((keyD.publicImage.address, 10)),
      changeAddress = addressA
    )

    balancesFor(addressA).Balances.Polys shouldBe Int128(999980)
    balancesFor(keyD.publicImage.address).Balances.Polys shouldBe Int128(10)
  }

  "Polys can be sent from addressD to a burner address" in {
    sendAndAwaitPolyTransaction(
      name = "tx-poly-d-to-burn-10",
      sender = NonEmptyChain(keyD.publicImage.address),
      recipients = NonEmptyChain((burnAddress, 10)),
      changeAddress = keyD.publicImage.address
    )

    balancesFor(burnAddress).Balances.Polys shouldBe Int128(10)
    balancesFor(keyD.publicImage.address).Balances.Polys shouldBe Int128(0)
  }

  "Arbits can be sent from addressA to addressB" in {
    sendAndAwaitArbitTransaction(
      name = "tx-arbit-a-to-b-10",
      sender = NonEmptyChain(addressA),
      recipients = NonEmptyChain((addressB, 10)),
      changeAddress = addressA
    )

    balancesFor(addressA).Balances.Arbits shouldBe Int128(999990)
    balancesFor(addressB).Balances.Arbits shouldBe Int128(1000010)
    balancesFor(addressC).Balances.Arbits shouldBe Int128(1000000)
  }

  "Arbits can be sent from addressA to addressD" in {

    sendAndAwaitArbitTransaction(
      name = "tx-arbit-a-to-d-10",
      sender = NonEmptyChain(addressA),
      recipients = NonEmptyChain((keyD.publicImage.address, 10)),
      changeAddress = addressA
    )

    balancesFor(addressA).Balances.Arbits shouldBe Int128(999980)
    balancesFor(keyD.publicImage.address).Balances.Arbits shouldBe Int128(10)

  }

  "Arbits can be sent from addressD to a burner address" in {
    // addressD currently has a 0-poly balance, so we need to give it some polys in order to pay the fee for the next transaction
    sendAndAwaitPolyTransaction(
      name = "tx-poly-c-to-d-10-2",
      sender = NonEmptyChain(addressC),
      recipients = NonEmptyChain((keyD.publicImage.address, 10)),
      changeAddress = addressC
    )

    sendAndAwaitArbitTransaction(
      name = "tx-arbit-d-to-burn-10",
      sender = NonEmptyChain(keyD.publicImage.address),
      recipients = NonEmptyChain((burnAddress, 10)),
      changeAddress = keyD.publicImage.address
    )

    balancesFor(burnAddress).Balances.Arbits shouldBe Int128(10)
    balancesFor(keyD.publicImage.address).Balances.Arbits shouldBe Int128(0)
  }

  def assetCode: AssetCode = AssetCode(1: Byte, addressC, "test_1")

  "Assets can be sent from addressC to addressA (minting)" in {
    sendAndAwaitAssetTransaction(
      name = "tx-asset-c-to-a-10",
      sender = NonEmptyChain(addressC),
      recipients = NonEmptyChain((addressA, AssetValue(10, assetCode))),
      changeAddress = addressC,
      minting = true
    )

    val List(assetBox) = balancesFor(addressA).Boxes("AssetBox").map { case a: AssetBox => a }

    assetBox.value.quantity shouldBe Int128(10)
    assetBox.value.assetCode shouldEqual assetCode
  }

  "Assets can be sent from addressA to addressB (non-minting)" in {
    sendAndAwaitAssetTransaction(
      name = "tx-asset-a-to-b-10",
      sender = NonEmptyChain(addressA),
      recipients = NonEmptyChain((addressB, AssetValue(10, assetCode))),
      changeAddress = addressA,
      minting = false
    )

    val List(assetBox) = balancesFor(addressB).Boxes("AssetBox").map { case a: AssetBox => a }

    assetBox.value.quantity shouldBe Int128(10)
    assetBox.value.assetCode shouldEqual assetCode
  }

  private def awaitBlocks(node: BifrostDockerNode)(count: Int): Unit = {
    val currentHeight =
      node.run(ToplRpc.NodeView.Head.rpc)(ToplRpc.NodeView.Head.Params()).value.height

    logger.info(s"Waiting $count blocks")
    node.pollUntilHeight(currentHeight + count).futureValue(Timeout((count * 4).seconds)).value
  }

  private def sendAndAwaitPolyTransaction(
    name:          String,
    sender:        NonEmptyChain[Address],
    recipients:    NonEmptyChain[(Address, Int128)],
    changeAddress: Address
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

    val signedTx = rawTx.copy(attestation = keyRing.generateAttestation(sender.iterator.toSet)(rawTx.messageToSign))

    broadcastAndAwait(name, signedTx)
  }

  private def sendAndAwaitArbitTransaction(
    name:          String,
    sender:        NonEmptyChain[Address],
    recipients:    NonEmptyChain[(Address, Int128)],
    changeAddress: Address
  ): Transaction.TX = {
    logger.info(s"Creating $name")
    val ToplRpc.Transaction.RawArbitTransfer.Response(rawTx, _) =
      node
        .run(ToplRpc.Transaction.RawArbitTransfer.rpc)(
          ToplRpc.Transaction.RawArbitTransfer.Params(
            propositionType = PublicKeyPropositionCurve25519.typeString,
            sender = sender,
            recipients = recipients,
            fee = 0,
            changeAddress = changeAddress,
            consolidationAddress = changeAddress,
            data = None
          )
        )
        .value

    val signedTx = rawTx.copy(attestation = keyRing.generateAttestation(sender.iterator.toSet)(rawTx.messageToSign))

    broadcastAndAwait(name, signedTx)
  }

  private def sendAndAwaitAssetTransaction(
    name:          String,
    sender:        NonEmptyChain[Address],
    recipients:    NonEmptyChain[(Address, AssetValue)],
    changeAddress: Address,
    minting:       Boolean
  ): Transaction.TX = {
    logger.info(s"Creating $name")
    val ToplRpc.Transaction.RawAssetTransfer.Response(rawTx, _) =
      node
        .run(ToplRpc.Transaction.RawAssetTransfer.rpc)(
          ToplRpc.Transaction.RawAssetTransfer.Params(
            propositionType = PublicKeyPropositionCurve25519.typeString,
            sender = sender,
            recipients = recipients,
            fee = 0,
            changeAddress = changeAddress,
            consolidationAddress = changeAddress,
            minting = minting,
            data = None
          )
        )
        .value

    val signedTx = rawTx.copy(attestation = keyRing.generateAttestation(sender.iterator.toSet)(rawTx.messageToSign))

    broadcastAndAwait(name, signedTx)
  }

  private def broadcastAndAwait(name: String, signedTx: Transaction.TX): Transaction.TX = {
    logger.info(s"Broadcasting signed $name")
    val broadcastedTx =
      node
        .run(ToplRpc.Transaction.BroadcastTx.rpc)(
          ToplRpc.Transaction.BroadcastTx.Params(signedTx)
        )
        .value

    broadcastedTx shouldEqual signedTx

    logger.info(s"Retrieving $name from mempool")
    val memPoolTx =
      node
        .run(ToplRpc.NodeView.TransactionFromMempool.rpc)(
          ToplRpc.NodeView.TransactionFromMempool.Params(broadcastedTx.id)
        )
        .value

    memPoolTx shouldEqual broadcastedTx

    awaitBlocks(node)(3)

    logger.info(s"Checking for $name in the chain")
    val ToplRpc.NodeView.TransactionById.Response(completedTransaction, _, _) =
      node.run(ToplRpc.NodeView.TransactionById.rpc)(ToplRpc.NodeView.TransactionById.Params(memPoolTx.id)).value

    completedTransaction shouldEqual memPoolTx

    logger.info(s"$name complete: $completedTransaction")

    completedTransaction

  }

  private def balancesFor(address: Address): ToplRpc.NodeView.Balances.Entry = {
    val balances =
      node.run(ToplRpc.NodeView.Balances.rpc)(ToplRpc.NodeView.Balances.Params(List(address))).value

    balances(address)
  }

}
