package co.topl.it

import cats.data.NonEmptyChain
import co.topl.attestation.AddressCodec.implicits._
import co.topl.utils.FromBytes.implicits._
import co.topl.attestation._
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.it.util._
import co.topl.modifier.box.{AssetCode, AssetValue}
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

//noinspection ScalaStyle
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
    val checksum = Blake2b256(bytes).take(AddressCodec.ChecksumLength)
    (bytes ++ checksum).decodeTo[AddressValidationError, Address].toEither.value
  }

  private var node: BifrostDockerNode = _
  private var addresses: List[Address] = _
  private def addressA: Address = addresses.head
  private def addressB: Address = addresses(1)
  private def addressC: Address = addresses(2)
  private def rewardsAddress: Address = addresses.last
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

    assignForgingAddress(node, rewardsAddress)
    keyD = keyRing.generateNewKeyPairs().success.value.head
  }

  "Polys can be sent from address A to addressB" in {
    verifyBalanceChange(addressA, -10, _.Balances.Polys) {
      verifyBalanceChange(addressB, 10, _.Balances.Polys) {
        verifyBalanceChange(addressC, 0, _.Balances.Polys) {
          sendAndAwaitPolyTransaction(
            name = "tx-poly-a-to-b-10",
            sender = NonEmptyChain(addressA),
            recipients = NonEmptyChain((addressB, 10)),
            changeAddress = addressA
          )
        }
      }
    }
  }

  "Polys can be sent from addressA to addressD" in {
    verifyBalanceChange(addressA, -10, _.Balances.Polys) {
      verifyBalanceChange(keyD.publicImage.address, 10, _.Balances.Polys) {
        sendAndAwaitPolyTransaction(
          name = "tx-poly-a-to-d-10",
          sender = NonEmptyChain(addressA),
          recipients = NonEmptyChain((keyD.publicImage.address, 10)),
          changeAddress = addressA
        )
      }
    }
  }

  "Polys can be sent from addressD to a burner address" in {
    verifyBalanceChange(keyD.publicImage.address, -10, _.Balances.Polys) {
      verifyBalanceChange(burnAddress, 10, _.Balances.Polys) {
        sendAndAwaitPolyTransaction(
          name = "tx-poly-d-to-burn-10",
          sender = NonEmptyChain(keyD.publicImage.address),
          recipients = NonEmptyChain((burnAddress, 10)),
          changeAddress = keyD.publicImage.address
        )
      }
    }
  }

  "Arbits can be sent from addressA to addressB" in {
    verifyBalanceChange(addressA, -10, _.Balances.Arbits) {
      verifyBalanceChange(addressB, 10, _.Balances.Arbits) {
        verifyBalanceChange(addressC, 0, _.Balances.Arbits) {
          sendAndAwaitArbitTransaction(
            name = "tx-arbit-a-to-b-10",
            sender = NonEmptyChain(addressA),
            recipients = NonEmptyChain((addressB, 10)),
            changeAddress = addressA,
            consolidationAddress = addressA
          )
        }
      }
    }
  }

  "Arbits can be sent from addressA to addressD" in {
    verifyBalanceChange(addressA, -10, _.Balances.Arbits) {
      verifyBalanceChange(keyD.publicImage.address, 10, _.Balances.Arbits) {
        sendAndAwaitArbitTransaction(
          name = "tx-arbit-a-to-d-10",
          sender = NonEmptyChain(addressA),
          recipients = NonEmptyChain((keyD.publicImage.address, 10)),
          changeAddress = addressA,
          consolidationAddress = addressA
        )
      }
    }
  }

  "Arbits can be sent from addressD to a burner address" in {
    // addressD currently has a 0-poly balance, so we need to give it some polys in order to pay the fee for the next transaction
    sendAndAwaitPolyTransaction(
      name = "tx-poly-c-to-d-10-2",
      sender = NonEmptyChain(addressC),
      recipients = NonEmptyChain((keyD.publicImage.address, 10)),
      changeAddress = addressC
    )

    verifyPolyEntropy(keyD.publicImage.address) {
      verifyBalanceChange(keyD.publicImage.address, -10, _.Balances.Arbits) {
        verifyBalanceChange(burnAddress, 10, _.Balances.Arbits) {
          sendAndAwaitArbitTransaction(
            name = "tx-arbit-d-to-burn-10",
            sender = NonEmptyChain(keyD.publicImage.address),
            recipients = NonEmptyChain((burnAddress, 10)),
            changeAddress = keyD.publicImage.address,
            consolidationAddress = keyD.publicImage.address
          )
        }
      }
    }
  }

  def assetCode: AssetCode = AssetCode(1: Byte, addressC, "test_1")

  "Assets can be sent from addressC to addressA (minting)" in {
    verifyPolyEntropy(addressC) {
      verifyBalanceChange(rewardsAddress, 1, _.Balances.Polys) {
        verifyBalanceChange(
          addressA,
          10,
          _.Boxes.AssetBox.find(_.value.assetCode == assetCode).fold(0: Int128)(_.value.quantity)
        ) {
          sendAndAwaitAssetTransaction(
            name = "tx-asset-c-to-a-10",
            sender = NonEmptyChain(addressC),
            recipients = NonEmptyChain((addressA, AssetValue(10, assetCode))),
            changeAddress = addressC,
            consolidationAddress = addressC,
            minting = true,
            fee = 1
          )
        }
      }
    }
  }

  "Assets can be sent from addressA to addressB (non-minting)" in {
    sendAndAwaitAssetTransaction(
      name = "tx-asset-a-to-b-10",
      sender = NonEmptyChain(addressA),
      recipients = NonEmptyChain((addressB, AssetValue(10, assetCode))),
      changeAddress = addressA,
      consolidationAddress = addressA,
      minting = false
    )

    val List(assetBox) = balancesFor(addressB).Boxes.AssetBox

    assetBox.value.quantity shouldBe Int128(10)
    assetBox.value.assetCode shouldEqual assetCode
  }

  private def awaitBlocks(node: BifrostDockerNode)(count: Int): Unit = {
    val currentHeight =
      node.run(ToplRpc.NodeView.Head.rpc)(ToplRpc.NodeView.Head.Params()).value.height

    logger.info(s"Waiting $count blocks")
    node.pollUntilHeight(currentHeight + count).futureValue(Timeout((count * 5).seconds)).value
  }

  private def sendAndAwaitPolyTransaction(
    name:          String,
    sender:        NonEmptyChain[Address],
    recipients:    NonEmptyChain[(Address, Int128)],
    changeAddress: Address,
    fee:           Int128 = 0
  ): Transaction.TX = {
    logger.info(s"Creating $name")
    val ToplRpc.Transaction.RawPolyTransfer.Response(rawTx, _) =
      node
        .run(ToplRpc.Transaction.RawPolyTransfer.rpc)(
          ToplRpc.Transaction.RawPolyTransfer.Params(
            propositionType = PublicKeyPropositionCurve25519.typeString,
            sender = sender,
            recipients = recipients,
            fee = fee,
            changeAddress = changeAddress,
            data = None
          )
        )
        .value

    val signedTx = rawTx.copy(attestation = keyRing.generateAttestation(sender.iterator.toSet)(rawTx.messageToSign))

    broadcastAndAwait(name, signedTx)
  }

  private def sendAndAwaitArbitTransaction(
    name:                 String,
    sender:               NonEmptyChain[Address],
    recipients:           NonEmptyChain[(Address, Int128)],
    changeAddress:        Address,
    consolidationAddress: Address,
    fee:                  Int128 = 0
  ): Transaction.TX = {
    logger.info(s"Creating $name")
    val ToplRpc.Transaction.RawArbitTransfer.Response(rawTx, _) =
      node
        .run(ToplRpc.Transaction.RawArbitTransfer.rpc)(
          ToplRpc.Transaction.RawArbitTransfer.Params(
            propositionType = PublicKeyPropositionCurve25519.typeString,
            sender = sender,
            recipients = recipients,
            fee = fee,
            changeAddress = changeAddress,
            consolidationAddress = consolidationAddress,
            data = None
          )
        )
        .value

    val signedTx = rawTx.copy(attestation = keyRing.generateAttestation(sender.iterator.toSet)(rawTx.messageToSign))

    broadcastAndAwait(name, signedTx)
  }

  private def sendAndAwaitAssetTransaction(
    name:                 String,
    sender:               NonEmptyChain[Address],
    recipients:           NonEmptyChain[(Address, AssetValue)],
    changeAddress:        Address,
    consolidationAddress: Address,
    minting:              Boolean,
    fee:                  Int128 = 0
  ): Transaction.TX = {
    logger.info(s"Creating $name")
    val ToplRpc.Transaction.RawAssetTransfer.Response(rawTx, _) =
      node
        .run(ToplRpc.Transaction.RawAssetTransfer.rpc)(
          ToplRpc.Transaction.RawAssetTransfer.Params(
            propositionType = PublicKeyPropositionCurve25519.typeString,
            sender = sender,
            recipients = recipients,
            fee = fee,
            changeAddress = changeAddress,
            consolidationAddress = consolidationAddress,
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

  /**
   * Compares the balance of the given address before and after the given test function
   * @param address The address to check
   * @param delta The expected change (Int128)
   * @param f A function which retrieves the value to check from a Balances Entry
   * @param test The test case to run (usually a transaction)
   * @return A wrapped test/Unit
   */
  private def verifyBalanceChange(address: Address, delta: Int128, f: ToplRpc.NodeView.Balances.Entry => Int128)(
    test:                                  => Any
  ) = {
    val initialBalances = balancesFor(address)
    val _ = test
    val finalBalances = balancesFor(address)
    f(finalBalances) shouldBe (f(initialBalances) + delta)
  }

  /**
   * Compares the poly boxes of the given address before and after the given test function and verifies
   * that they change
   * @param address The address to check
   * @param test The test case to run (usually a transaction)
   * @return A wrapped test/Unit
   */
  private def verifyPolyEntropy(address: Address)(test: => Any) = {
    val List(initialPolyBox) = balancesFor(address).Boxes.PolyBox
    val _ = test
    val List(finalPolyBox) = balancesFor(address).Boxes.PolyBox

    initialPolyBox should not equal finalPolyBox
    initialPolyBox.nonce should not equal finalPolyBox.nonce
  }

}
