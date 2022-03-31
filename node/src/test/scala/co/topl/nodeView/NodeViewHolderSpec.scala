package co.topl.nodeView

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.ActorRef
import akka.actor.typed.eventstream.EventStream
import cats.data.NonEmptyChain
import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.consensus.{ActorConsensusInterface, NxtConsensus, NxtLeaderElection, ValidBlockchainGenerator}
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.builder.TransferRequests.PolyTransferRequest
import co.topl.modifier.transaction.builder.{BoxSelectionAlgorithms, TransferBuilder}
import co.topl.nodeView.NodeViewHolder.ReceivableMessages
import co.topl.nodeView.NodeViewHolderSpec.TestInWithActor
import co.topl.nodeView.NodeViewTestHelpers.TestIn
import co.topl.nodeView.history.InMemoryKeyValueStore
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.{InMemoryKeyRingTestHelper, NodeGenerators, TestSettings, TimeProvider}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.{EitherValues, Inspectors, OptionValues}

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._

class NodeViewHolderSpec
    extends ScalaTestWithActorTestKit
    with AnyFlatSpecLike
    with InMemoryKeyRingTestHelper
    with NodeGenerators
    with ValidBlockchainGenerator
    with TestSettings
    with NodeViewTestHelpers
    with MockFactory
    with OptionValues
    with EitherValues
    with Inspectors {

  behavior of "NodeViewHolder"

//  it should "write, read, and remove transactions" in {
//
//    implicit val timeProvider: TimeProvider = mock[TimeProvider]
//
//    (() => timeProvider.time)
//      .expects()
//      .anyNumberOfTimes()
//      .onCall(() => System.currentTimeMillis())
//
//    genesisActorTest { testIn =>
//      val addressA :: addressB :: _ = keyRingCurve25519.addresses.toList
//
//      val unsignedPolyTransfer =
//        TransferBuilder
//          .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](
//            testIn.testIn.nodeView.state,
//            PolyTransferRequest(
//              List(addressB),
//              List(addressA -> 10),
//              addressB,
//              0,
//              None
//            ),
//            BoxSelectionAlgorithms.All
//          )
//          .getOrThrow()
//
//      val polyTransfer =
//        unsignedPolyTransfer.copy(attestation =
//          keyRingCurve25519.generateAttestation(addressB)(unsignedPolyTransfer.messageToSign)
//        )
//
//      val transactions = List(polyTransfer)
//      testIn.nodeViewHolderRef.tell(NodeViewHolder.ReceivableMessages.WriteTransactions(transactions))
//      Thread.sleep(1.seconds.toMillis)
//      testIn.testIn.nodeView.mempool.modifierById(transactions.head.id).value shouldBe transactions.head
//
//      testIn.nodeViewHolderRef.tell(NodeViewHolder.ReceivableMessages.EliminateTransactions(transactions.map(_.id)))
//      Thread.sleep(1.seconds.toMillis)
//      testIn.testIn.nodeView.mempool.modifierById(transactions.head.id) shouldBe None
//    }
//  }

  it should "write a single viable block" in {

    implicit val timeProvider: TimeProvider = mock[TimeProvider]

    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    withGenesisOnlyNVHActor { testInWithActors =>
      val nextBlocks =
        validChainFromGenesis(keyRingCurve25519, Int.MaxValue, Long.MaxValue, protocolVersioner)(
          3
        ).sample.get.tail.toChain.toList

      implicit val executionContext: ExecutionContextExecutor = testKit.system.executionContext

      testInWithActors.nodeViewHolderInterface.applyBlocks(nextBlocks).value.futureValue.value

      testInWithActors.nodeViewHolderInterface
        .withNodeView(identity)
        .map { view =>
//          println(s">>>>>>>> Genesis block id: ${testInWithActors.testIn.genesisView.block.id}")
//          println(s">>>>>> one block append: ${view.history.bestBlockId}")
          forAll(nextBlocks) { block =>
//            println(
//              s"checking blockId: ${block.id}, ${view.history.modifierById(block.id)}"
//            )
            view.history.modifierById(block.id) shouldBe Some
          }
        }
        .value
        .futureValue
        .value
    }
  }
//
//  it should "write multiple viable blocks" in {
//
//    implicit val timeProvider: TimeProvider = mock[TimeProvider]
//
//    (() => timeProvider.time)
//      .expects()
//      .anyNumberOfTimes()
//      .onCall(() => System.currentTimeMillis())
//
//    withGenesisOnlyNVHActor { testInWithActors =>
//      val nextBlocks =
//        validChainFromGenesis(keyRingCurve25519, Int.MaxValue, Long.MaxValue, protocolVersioner)(
//          3
//        ).sample.get.tail.toChain.toList
//
//      implicit val executionContext: ExecutionContextExecutor = testKit.system.executionContext
//      testInWithActors.nodeViewHolderInterface.applyBlocks(nextBlocks).value.map { _ =>
//        forAll(nextBlocks) { block =>
//          println(
//            s"checking blockId: ${block.id}, ${testInWithActors.testIn.nodeView.history.modifierById(block.id).isDefined}"
//          )
//          testInWithActors.testIn.nodeView.history.modifierById(block.id) shouldBe Some
//        }
//      }
//    }
//  }

//  it should "cache non-viable blocks" in {
//
//    implicit val timeProvider: TimeProvider = mock[TimeProvider]
//
//    (() => timeProvider.time)
//      .expects()
//      .anyNumberOfTimes()
//      .onCall(() => System.currentTimeMillis())
//
//    genesisActorTest { testInWithActors =>
//      val genesisBlock = testInWithActors.testIn.genesisView.block
//      val leaderElection = new NxtLeaderElection(protocolVersioner)
//      val nextBlocks = generateBlockExtensions(
//        genesisBlock,
//        List(genesisBlock),
//        keyRingCurve25519.addresses.head,
//        leaderElection
//      ).take(3).toList
//      // Insert blocks 2 and 3, but not block 1
//      testInWithActors.nodeViewHolderRef.tell(ReceivableMessages.WriteBlocks(nextBlocks.takeRight(2)))
//      Thread.sleep(2.seconds.toMillis)
//      // Because block 1 is not available yet, blocks 2 and 3 should not be in history yet either
//      forAll(nextBlocks) { block =>
//        testInWithActors.testIn.nodeView.history.modifierById(block.id) shouldBe None
//      }
//      // Now write block 1
//      testInWithActors.nodeViewHolderRef.tell(ReceivableMessages.WriteBlocks(nextBlocks.take(1)))
//      Thread.sleep(2.seconds.toMillis)
//      // And verify that block 1 was written, as well as blocks 2 and 3 from the previous attempt
//      forAll(nextBlocks) { block =>
//        testInWithActors.testIn.nodeView.history.modifierById(block.id).value
//      }
//    }
//  }
//
//  it should "Reject blocks containing transactions already in history" in {
//    val txs = getHistory.bestBlock.transactions
//
//    txs.foreach(tx => actorSystem.eventStream.tell(EventStream.Publish(???)))
//    txs.foreach(tx => getMempool.contains(tx) shouldBe false)
//
//    implicit val timeProvider: TimeProvider = mock[TimeProvider]
//
//    (() => timeProvider.time)
//      .expects()
//      .anyNumberOfTimes()
//      .onCall(() => System.currentTimeMillis())
//
//    genesisActorTest { testInWithActors =>
//      val genesisBlock = testInWithActors.testIn.genesisView.block
//      val nextBlocks = validChainFromGenesis(keyRingCurve25519, Int.MaxValue, Long.MaxValue, protocolVersioner)(10)
//
//      testInWithActors.nodeViewHolderRef.tell(ReceivableMessages.WriteBlocks(nextBlocks))
//
//      Thread.sleep(2.seconds.toMillis)
//      forAll(nextBlocks) { block =>
//        testInWithActors.testIn.nodeView.history.modifierById(block.id).value
//      }
//    }
//  }

  private def withGenesisOnlyNVHActor(test: TestInWithActor => Unit)(implicit timeProvider: TimeProvider): Unit =
    testInWithActor(nodeViewGenesisOnlyTestInputs(nxtConsensusGenesisGen.sample.get))(test)

  private def withValidChainNVHActor(lengthOfChain: Byte)(test: TestInWithActor => Unit)(implicit
    timeProvider:                                   TimeProvider
  ): Unit =
    testInWithActor(nodeViewValidChainTestInputs(lengthOfChain))(test)

  private def testInWithActor(
    testIn: TestIn
  )(test:   TestInWithActor => Unit)(implicit timeProvider: TimeProvider): Unit = {
    val consensusStorageRef = spawn(
      NxtConsensus(
        TestSettings.defaultSettings,
        InMemoryKeyValueStore.empty()
      )
    )
    val nodeViewHolderRef = spawn(
      NodeViewHolder(
        TestSettings.defaultSettings,
        new ActorConsensusInterface(consensusStorageRef),
        () => Future.successful(testIn.nodeView)
      )
    )
    val testInWithActor = TestInWithActor(
      testIn,
      new ActorNodeViewHolderInterface(nodeViewHolderRef),
      new ActorConsensusInterface(consensusStorageRef)
    )
    test(testInWithActor)
//    testKit.stop(nodeViewHolderRef)
//    testKit.stop(consensusStorageRef)
  }
}

object NodeViewHolderSpec {

  case class TestInWithActor(
    testIn:                  TestIn,
    nodeViewHolderInterface: ActorNodeViewHolderInterface,
    consensusInterface:      ActorConsensusInterface
  )
}
