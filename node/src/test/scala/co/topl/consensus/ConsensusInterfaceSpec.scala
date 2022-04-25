package co.topl.consensus

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.ActorRef
import akka.actor.typed.eventstream.EventStream
import akka.pattern.StatusReply
import cats.data.NonEmptyChain
import co.topl.attestation.Address
import co.topl.consensus.ConsensusInterfaceSpec.TestInWithActor
import co.topl.consensus.NxtConsensus.ReceivableMessages.{ReadState, RollbackState}
import co.topl.consensus.NxtConsensus.State
import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.transaction.TransferTransaction
import co.topl.nodeView.NodeViewHolder
import co.topl.nodeView.history.InMemoryKeyValueStore
import co.topl.utils._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationDouble

class ConsensusInterfaceSpec
    extends ScalaTestWithActorTestKit
    with AnyFlatSpecLike
    with TestSettings
    with ValidBlockchainGenerator
    with InMemoryKeyRingTestHelper
    with NodeGenerators
    with MockFactory
    with OptionValues {

  behavior of "ConsensusStorage"

  implicit def ec: ExecutionContext = system.executionContext

  it should "have been updated with genesis state since we are manually updating it in withFreshView for testing" in {
    implicit val timeProvider: TimeProvider = mock[TimeProvider]
    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    consensusActorTest { testInWithActors =>
      val probe = createTestProbe[StatusReply[State]]()
      Thread.sleep(0.1.seconds.toMillis)
      testInWithActors.consensusViewRef ! ReadState(probe.ref)
      probe.expectMessage(StatusReply.success(testInWithActors.genesis.state))
    }
  }

  it should "update the consensus params when there is a new block published" in {
    implicit val timeProvider: TimeProvider = mock[TimeProvider]
    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    consensusActorTest { testInWithActors =>
      val probe = createTestProbe[StatusReply[State]]()
      // omitting the generated genesis from blockchainGen
      val newBlocks = blockchainGen((settings.application.consensusStoreVersionsToKeep / 2).toByte).sample.value.tail
      Thread.sleep(0.1.seconds.toMillis)
      newBlocks.map { block =>
        system.eventStream.tell(EventStream.Publish(NodeViewHolder.Events.SemanticallySuccessfulModifier(block)))
      }
      Thread.sleep(0.1.seconds.toMillis)
      testInWithActors.consensusViewRef ! ReadState(probe.ref)
      // Increasing the newBlock number by one as the height since we start out with a genesis block
      probe.expectMessage(
        StatusReply.success(
          NxtConsensus.State(
            testInWithActors.genesis.state.totalStake,
            newBlocks.last.difficulty,
            0L,
            newBlocks.length + 1
          )
        )
      )
    }
  }

  it should "load total stake from storage on start" in {

    implicit val timeProvider: TimeProvider = mock[TimeProvider]

    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    val probe = createTestProbe[StatusReply[State]]()
    val store = InMemoryKeyValueStore.empty
    val consensusStorageRef = spawn(
      NxtConsensus(settings, store),
      NxtConsensus.actorName
    )
    // omitting the generated genesis from blockchainGen
    val newBlocks = blockchainGen((settings.application.consensusStoreVersionsToKeep / 2).toByte).sample.value.tail

    Thread.sleep(0.1.seconds.toMillis)
    newBlocks.map { block =>
      system.eventStream.tell(EventStream.Publish(NodeViewHolder.Events.SemanticallySuccessfulModifier(block)))
    }
    Thread.sleep(0.1.seconds.toMillis)
    consensusStorageRef ! ReadState(probe.ref)
    val params = probe.receiveMessage(0.1.seconds)
    testKit.stop(consensusStorageRef)

    // initialize a new consensus actor with the modified InMemoryKeyValueStore
    val newConsensusStorageRef = spawn(
      NxtConsensus(settings, store),
      NxtConsensus.actorName
    )
    newConsensusStorageRef ! ReadState(probe.ref)
    probe.expectMessage(params)
    testKit.stop(newConsensusStorageRef)
  }

  it should "roll back to a previous version" in {
    implicit val timeProvider: TimeProvider = mock[TimeProvider]
    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    consensusActorTest { testInWithActors =>
      val probe = createTestProbe[StatusReply[State]]()
      // omitting the generated genesis from blockchainGen
      val newBlocks = blockchainGen((settings.application.consensusStoreVersionsToKeep / 2).toByte).sample.value.tail

      Thread.sleep(0.1.seconds.toMillis)

      newBlocks.map { block =>
        system.eventStream.tell(EventStream.Publish(NodeViewHolder.Events.SemanticallySuccessfulModifier(block)))
      }

      Thread.sleep(0.1.seconds.toMillis)

      testInWithActors.consensusViewRef ! RollbackState(newBlocks.head.id, probe.ref)
      // the first of the newBlocks would be at height 2 since it's the first one after the genesis block
      probe.expectMessage(
        StatusReply.success(
          State(blockTotalStake(testInWithActors.genesis.block), newBlocks.head.difficulty, 0L, 2L)
        )
      )
    }
  }

  it should "fail to roll back to a version beyond the number of versions to keep" in {
    implicit val timeProvider: TimeProvider = mock[TimeProvider]
    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    consensusActorTest { testInWithActors =>
      val probe = createTestProbe[StatusReply[State]]()
      // plus 2 to exceed the versionsToKeep since we are omitting the generated genesis by doing .tail
      val newBlocks = blockchainGen((settings.application.consensusStoreVersionsToKeep + 2).toByte).sample.value.tail

      Thread.sleep(0.1.seconds.toMillis)

      newBlocks.map { block =>
        system.eventStream.tell(EventStream.Publish(NodeViewHolder.Events.SemanticallySuccessfulModifier(block)))
      }

      Thread.sleep(0.1.seconds.toMillis)

      testInWithActors.consensusViewRef ! RollbackState(newBlocks.head.id, probe.ref)
      probe.receiveMessage(1.seconds).toString() shouldEqual "Error(Failed to roll back to the given version)"
    }
  }

  private def consensusActorTest(test: TestInWithActor => Unit)(implicit timeProvider: TimeProvider): Unit = {
    val addresses: Set[Address] =
      nonEmptySetAddressGen.sample.get.take(settings.forging.addressGenerationSettings.numberOfAddresses)
    val genesis: NxtConsensus.Genesis =
      new GenesisProvider(protocolVersioner.applicable(1).blockVersion, addresses)
        .fetchGenesis(settings)
        .toOption
        .value

    val consensusRef =
      spawn(
        NxtConsensus(
          settings,
          InMemoryKeyValueStore(settings.application.consensusStoreVersionsToKeep)
        ),
        NxtConsensus.actorName
      )
    val consensusInterface = new ActorConsensusInterface(consensusRef)
    // update the genesis state to consensus since we are not spinning up a nodeViewHolder which does the initial update
    consensusInterface.update(
      genesis.block.id,
      NxtConsensus
        .StateUpdate(Some(genesis.state.totalStake), Some(genesis.block.difficulty), None, Some(genesis.block.height))
    )

    val testInWithActor = TestInWithActor(genesis, consensusRef)
    test(testInWithActor)
    testKit.stop(consensusRef)
  }

  private def blockchainGen: Byte => Gen[GenesisHeadChain] =
    (length: Byte) =>
      validChainFromGenesis(
        keyRingCurve25519,
        settings.application.genesis.generated.value,
        protocolVersioner
      )(length)

  private def blockTotalStake(block: Block) =
    block.transactions
      .collect { case transaction: TransferTransaction[_, _] =>
        transaction.newBoxes.collect { case box: ArbitBox => box.value.quantity }.toSeq
      }
      .flatten
      .sum
}

object ConsensusInterfaceSpec {

  case class TestInWithActor(
    genesis:          NxtConsensus.Genesis,
    consensusViewRef: ActorRef[NxtConsensus.ReceivableMessage]
  )
}
