package co.topl.it

import cats.data.NonEmptyChain
import co.topl.attestation._
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.it.util._
import co.topl.modifier.box.{AssetCode, AssetValue}
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.builder.BoxSelectionAlgorithms
import co.topl.rpc.ToplRpc
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data
import com.typesafe.config.ConfigFactory
import io.circe.syntax._
import org.scalatest._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

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
           |bifrost.forging.addressGenerationSettings.strategy = fromSeed
           |bifrost.forging.addressGenerationSettings.addressSeedOpt = "$nodeGroupName"
           |bifrost.forging.forgeOnStartup = false
           |""".stripMargin
    )

  private val burnAddress = addressFromBytes(Array.fill(32)(2: Byte))

  private var node: BifrostDockerNode = _
  private var addresses: List[Address] = _
  private def addressA: Address = addresses.head
  private def addressB: Address = addresses(1)
  private def addressC: Address = addresses(2)
  private def rewardsAddress: Address = addresses.last
  private var keyD: PrivateKeyCurve25519 = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    node = dockerSupport.createNode("e2eTransactionBuildAndSubmitTestNode", nodeGroupName)
    node.reconfigure(nodeConfig)
    node.start()
    node.waitForStartup().futureValue(Timeout(30.seconds)).value

    logger.info("Fetching addresses from Node")
    addresses =
      node.run(ToplRpc.Admin.ListOpenKeyfiles.rpc)(ToplRpc.Admin.ListOpenKeyfiles.Params()).value.unlocked.toList
    keyRing.addresses shouldBe addresses.toSet

    assignForgingAddress(node, rewardsAddress)
    keyD = keyRing.generateNewKeyPairs().success.value.head
    node.run(ToplRpc.Admin.StartForging.rpc)(ToplRpc.Admin.StartForging.Params()).value
  }

  "Polys can be sent from address A to addressB" in {
    verifyBalanceChange(addressA, -10, _.Balances.Polys) {
      verifyBalanceChange(addressB, 10, _.Balances.Polys) {
        verifyBalanceChange(addressC, 0, _.Balances.Polys) {
          sendAndAwaitPolyTransaction(
            label = "tx-poly-a-to-b-10",
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
          label = "tx-poly-a-to-d-10",
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
          label = "tx-poly-d-to-burn-10",
          sender = NonEmptyChain(keyD.publicImage.address),
          recipients = NonEmptyChain((burnAddress, 10)),
          changeAddress = keyD.publicImage.address
        )
      }
    }
  }

  "Polys can be sent from addressA to addressB with a fee" in {
    verifyBalanceChange(addressA, -15, _.Balances.Polys) {
      verifyBalanceChange(addressB, 10, _.Balances.Polys) {
        verifyBalanceChange(rewardsAddress, 5, _.Balances.Polys) {
          sendAndAwaitPolyTransaction(
            label = "tx-poly-a-to-b-10-with-fee",
            sender = NonEmptyChain(addressA),
            recipients = NonEmptyChain((addressB, 10)),
            changeAddress = addressA,
            fee = 5
          )
        }
      }
    }
  }

  "Change from a poly transaction should go to the change address" in {
    val prev_bal: Map[String, Int128] = Map(
      "a_poly" -> balancesFor(addressA).Balances.Polys,
      "b_poly" -> balancesFor(addressB).Balances.Polys,
      "c_poly" -> balancesFor(addressC).Balances.Polys
    )

    val send_amount = 10
    val change_amount = prev_bal("c_poly") - send_amount

    // Now return the polys back to addressC to allow subsequent tests to work
    verifyBalanceChange(addressA, -change_amount, _.Balances.Polys) {
      verifyBalanceChange(addressB, -send_amount, _.Balances.Polys) {
        verifyBalanceChange(addressC, prev_bal("c_poly"), _.Balances.Polys) {
          verifyBalanceChange(addressA, change_amount, _.Balances.Polys) {
            verifyBalanceChange(addressB, send_amount, _.Balances.Polys) {
              verifyBalanceChange(addressC, -prev_bal("c_poly"), _.Balances.Polys) {
                sendAndAwaitPolyTransaction(
                  label = s"tx-poly_send-c-to-b-with-a-change",
                  tx_io_desc = Some(
                    Map(
                      "inputs" -> List(
                        addressC -> prev_bal("c_poly")
                      ),
                      "outputs" -> List(
                        addressB -> send_amount,
                        addressA -> change_amount
                      )
                    )
                  ),
                  sender = NonEmptyChain(addressC),
                  recipients = NonEmptyChain((addressB, send_amount)),
                  changeAddress = addressA
                )
              }
            }
          }

          sendAndAwaitPolyTransaction(
            label = s"tx-poly_refund-a-to-c",
            tx_io_desc = Some(
              Map(
                "inputs" -> List(
                  addressA -> (prev_bal("a_poly") + change_amount)
                ),
                "outputs" -> List(
                  addressC -> change_amount,
                  addressA -> prev_bal("a_poly")
                )
              )
            ),
            sender = NonEmptyChain(addressA),
            recipients = NonEmptyChain((addressC, change_amount)),
            changeAddress = addressA
          )
          sendAndAwaitPolyTransaction(
            label = s"tx-poly_refund-b-to-c",
            tx_io_desc = Some(
              Map(
                "inputs" -> List(
                  addressB -> (prev_bal("b_poly") + send_amount)
                ),
                "outputs" -> List(
                  addressC -> send_amount,
                  addressB -> prev_bal("b_poly")
                )
              )
            ),
            sender = NonEmptyChain(addressB),
            recipients = NonEmptyChain((addressC, send_amount), (addressB, prev_bal("b_poly"))),
            changeAddress = addressB
          )
        }
      }
    }
  }

  "Arbits can be sent from addressA to addressB" in {
    verifyBalanceChange(addressA, -10, _.Balances.Arbits) {
      verifyBalanceChange(addressB, 10, _.Balances.Arbits) {
        verifyBalanceChange(addressC, 0, _.Balances.Arbits) {
          sendAndAwaitArbitTransaction(
            label = "tx-arbit-a-to-b-10",
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
          label = "tx-arbit-a-to-d-10",
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
      label = "tx-poly-c-to-d-10-2",
      sender = NonEmptyChain(addressC),
      recipients = NonEmptyChain((keyD.publicImage.address, 10)),
      changeAddress = addressC
    )

    verifyPolyEntropy(keyD.publicImage.address) {
      verifyBalanceChange(keyD.publicImage.address, -10, _.Balances.Arbits) {
        verifyBalanceChange(burnAddress, 10, _.Balances.Arbits) {
          sendAndAwaitArbitTransaction(
            label = "tx-arbit-d-to-burn-10",
            sender = NonEmptyChain(keyD.publicImage.address),
            recipients = NonEmptyChain((burnAddress, 10)),
            changeAddress = keyD.publicImage.address,
            consolidationAddress = keyD.publicImage.address
          )
        }
      }
    }
  }

  "Arbits can be sent from addressA to addressB with a fee" in {
    verifyBalanceChange(addressA, -5, _.Balances.Polys) {
      verifyBalanceChange(addressA, -10, _.Balances.Arbits) {
        verifyBalanceChange(addressB, 10, _.Balances.Arbits) {
          verifyBalanceChange(rewardsAddress, 5, _.Balances.Polys) {
            sendAndAwaitArbitTransaction(
              label = "tx-arbit-a-to-b-10-with-fee",
              sender = NonEmptyChain(addressA),
              recipients = NonEmptyChain((addressB, 10)),
              changeAddress = addressA,
              consolidationAddress = addressA,
              fee = 5
            )
          }
        }
      }
    }
  }

  def assetCode: AssetCode = AssetCode(1: Byte, addressC, Latin1Data.unsafe("test_1"))

  "Assets can be sent from addressC to addressA (minting)" in {
    verifyPolyEntropy(addressC) {
      verifyBalanceChange(rewardsAddress, 1, _.Balances.Polys) {
        verifyBalanceChange(
          addressA,
          10,
          _.Boxes.AssetBox.find(_.value.assetCode == assetCode).fold(0: Int128)(_.value.quantity)
        ) {
          sendAndAwaitAssetTransaction(
            label = "tx-asset-c-to-a-10",
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
      label = "tx-asset-a-to-b-10",
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

  "Fees are sent to the proper address after a forger address update" in {
    val newRewardsAddress =
      node.run(ToplRpc.Admin.GenerateKeyfile.rpc)(ToplRpc.Admin.GenerateKeyfile.Params("rewards")).value.address

    node.run(ToplRpc.Admin.UpdateRewardsAddress.rpc)(ToplRpc.Admin.UpdateRewardsAddress.Params(newRewardsAddress)).value

    verifyBalanceChange(addressA, -15, _.Balances.Polys) {
      verifyBalanceChange(addressB, 10, _.Balances.Polys) {
        verifyBalanceChange(newRewardsAddress, 5, _.Balances.Polys) {
          sendAndAwaitPolyTransaction(
            label = "forger-address-change",
            sender = NonEmptyChain(addressA),
            recipients = NonEmptyChain((addressB, 10)),
            changeAddress = addressA,
            fee = 5
          )
        }
      }
    }

    // Double-check the rewards balance
    balancesFor(newRewardsAddress).Balances.Polys shouldBe Int128(5)
  }

  private def sendAndAwaitPolyTransaction(
    label:         String,
    tx_io_desc:    Option[Map[String, List[(Address, Int128)]]] = None,
    sender:        NonEmptyChain[Address],
    recipients:    NonEmptyChain[(Address, Int128)],
    changeAddress: Address,
    fee:           Int128 = 0
  ): Transaction.TX = {
    logger.info(s"Creating $label")
    if (tx_io_desc.nonEmpty) {
      logger.info(
        s"\n inputs : List[(Address, Int128)] =\t ${tx_io_desc.get("inputs")}" +
        s"\n outputs: List[(Address, Int128)] =\t ${tx_io_desc.get("outputs")}"
      )
    }
    val unprovenTransactionFromBuilder =
      node
        .run(ToplRpc.Transaction.RawPolyTransfer.rpc)(
          ToplRpc.Transaction.RawPolyTransfer.Params(
            propositionType = PublicKeyPropositionCurve25519.typeString,
            sender = sender,
            recipients = recipients,
            fee = fee,
            changeAddress = changeAddress,
            data = None,
            boxSelectionAlgorithm = BoxSelectionAlgorithms.All
          )
        )

    // should assert that my transaction matches the message to sign in their transaction

    unprovenTransactionFromBuilder.toOption match {
      case Some(res) =>
        val signedTx =
          res.rawTx.copy(attestation = keyRing.generateAttestation(sender.iterator.toSet)(res.rawTx.messageToSign))
        println(s">>>>>>>>> sending transaction ${signedTx.asJson}")
        broadcastAndAwait(label, signedTx)
      case _ => throw new Exception(s"Failed to process tx label=$label")
    }
  }

  private def sendAndAwaitArbitTransaction(
    label:                String,
    tx_io_desc:           Option[Map[String, List[(Address, Int128)]]] = None,
    sender:               NonEmptyChain[Address],
    recipients:           NonEmptyChain[(Address, Int128)],
    changeAddress:        Address,
    consolidationAddress: Address,
    fee:                  Int128 = 0
  ): Transaction.TX = {
    logger.info(s"Creating $label")
    if (tx_io_desc.nonEmpty) {
      logger.info(
        s"\n inputs : List[(Address, Int128)] =\t ${tx_io_desc.get("inputs")}" +
        s"\n outputs: List[(Address, Int128)] =\t ${tx_io_desc.get("outputs")}"
      )
    }
    val unprovenTransactionFromBuilder =
      node
        .run(ToplRpc.Transaction.RawArbitTransfer.rpc)(
          ToplRpc.Transaction.RawArbitTransfer.Params(
            propositionType = PublicKeyPropositionCurve25519.typeString,
            sender = sender,
            recipients = recipients,
            fee = fee,
            changeAddress = changeAddress,
            consolidationAddress = consolidationAddress,
            data = None,
            boxSelectionAlgorithm = BoxSelectionAlgorithms.All
          )
        )

    // should assert that my transaction matches the message to sign in their transaction

    unprovenTransactionFromBuilder.toOption match {
      case Some(res) =>
        val signedTx =
          res.rawTx.copy(attestation = keyRing.generateAttestation(sender.iterator.toSet)(res.rawTx.messageToSign))
        broadcastAndAwait(label, signedTx)
      case _ => throw new Exception(s"Failed to process tx label=$label")
    }
  }

  private def sendAndAwaitAssetTransaction(
    label:                String,
    tx_io_desc:           Option[Map[String, List[(Address, Int128)]]] = None,
    sender:               NonEmptyChain[Address],
    recipients:           NonEmptyChain[(Address, AssetValue)],
    changeAddress:        Address,
    consolidationAddress: Address,
    minting:              Boolean,
    fee:                  Int128 = 0
  ): Transaction.TX = {
    logger.info(s"Creating $label")
    if (tx_io_desc.nonEmpty) {
      logger.info(
        s"\n inputs : List[(Address, Int128)] =\t ${tx_io_desc.get("inputs")}" +
        s"\n outputs: List[(Address, Int128)] =\t ${tx_io_desc.get("outputs")}"
      )
    }
    val unprovenTransactionFromBuilder =
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
            data = None,
            boxSelectionAlgorithm = BoxSelectionAlgorithms.All
          )
        )

    // should assert that my transaction matches the message to sign in their transaction

    unprovenTransactionFromBuilder.toOption match {
      case Some(res) =>
        val signedTx =
          res.rawTx.copy(attestation = keyRing.generateAttestation(sender.iterator.toSet)(res.rawTx.messageToSign))
        broadcastAndAwait(label, signedTx)
      case _ => throw new Exception(s"Failed to process tx label=$label")
    }
  }

  private def broadcastAndAwait(name: String, signedTx: Transaction.TX): Transaction.TX = {
    logger.info(s"Broadcasting signed $name id=${signedTx.id}")
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

    logger.info(s"Checking for $name in the chain")
    val ToplRpc.NodeView.TransactionById.Response(completedTransaction, _, _) =
      node.pollForTransaction(memPoolTx.id).value.futureValue(Timeout(30.seconds)).value

    completedTransaction shouldEqual memPoolTx

    logger.info(s"Verifying $name no longer in mempool")
    node
      .run(ToplRpc.NodeView.TransactionFromMempool.rpc)(
        ToplRpc.NodeView.TransactionFromMempool.Params(broadcastedTx.id)
      )
      .left
      .value

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
