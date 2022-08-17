package co.topl.consensus

import co.topl.attestation.Address
import co.topl.consensus.KeyManager.KeyView
import co.topl.modifier.{ModifierId, ProgramId}
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, SimpleValue}
import co.topl.modifier.transaction.Transaction
import co.topl.network.BifrostSyncInfo
import co.topl.nodeView.ReadableNodeView
import co.topl.nodeView.history.HistoryReader
import co.topl.nodeView.mempool.{MemPoolReader, UnconfirmedTx}
import co.topl.nodeView.state.StateReader
import co.topl.utils._
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpecLike
import org.scalatest.{BeforeAndAfterAll, EitherValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import org.slf4j.Logger

class ForgeSpec
    extends AnyPropSpecLike
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with MockFactory
    with InMemoryKeyRingTestHelper
    with TestSettings
    with BeforeAndAfterAll
    with CommonGenerators
    with Matchers {

  implicit private def implicitLogger: Logger = logger.underlying

  property("successfully create from a ReadableNodeView") {
    forAll(blockCurve25519Gen.map(_.copy(timestamp = 1))) { parentBlock =>
      implicit val timeProvider: TimeProvider = mock[TimeProvider]
      val nodeView = ReadableNodeView(
        mock[HistoryReader[Block, BifrostSyncInfo]],
        mock[StateReader[ProgramId, Address]],
        mock[MemPoolReader[Transaction.TX]]
      )

      (nodeView.memPool
        .take[Int128](_: Int)(_: UnconfirmedTx[Transaction.TX] => Int128)(_: Ordering[Int128]))
        .expects(100, *, *)
        .once()
        .returning(Nil)

      (() => timeProvider.time)
        .expects()
        .once()
        // Big time delta allows LeaderElection to find an eligible box
        .returning(Long.MaxValue)

      (() => nodeView.history.bestBlock)
        .expects()
        .once()
        .returning(parentBlock)

      (nodeView.history
        .getTimestampsFrom(_: Block, _: Long))
        .expects(parentBlock, 3)
        .returning(Vector(parentBlock.timestamp))

      (nodeView.history
        .consensusStateAt(_: ModifierId))
        .expects(parentBlock.id)
        .returning(Right(NxtConsensus.State(10000000L, 0)))

      val rewardsAddress = keyRingCurve25519.addresses.head

      (nodeView.state
        .getTokenBoxes(_: Address))
        .expects(*)
        .anyNumberOfTimes()
        .onCall((a: Address) => Some(List(ArbitBox(a.evidence, nonce = Long.MaxValue, value = SimpleValue(1)))))

      val keyView =
        KeyView(
          keyRingCurve25519.addresses,
          Some(rewardsAddress),
          keyRingCurve25519.signWithAddress,
          keyRingCurve25519.lookupPublicKey
        )

      val forge =
        Forge
          .prepareForge(
            nodeView,
            keyView,
            0
          )
          .value

      val block = forge.make.value
      block.parentId shouldBe parentBlock.id
      block.timestamp shouldBe Long.MaxValue
    }
  }

  property("fail to create if the KeyView does not contain a rewards address") {
    implicit val timeProvider: TimeProvider = mock[TimeProvider]
    val nodeView = ReadableNodeView(
      mock[HistoryReader[Block, BifrostSyncInfo]],
      mock[StateReader[ProgramId, Address]],
      mock[MemPoolReader[Transaction.TX]]
    )
    val keyView =
      KeyView(keyRingCurve25519.addresses, None, keyRingCurve25519.signWithAddress, keyRingCurve25519.lookupPublicKey)

    Forge
      .prepareForge(
        nodeView,
        keyView,
        0
      )
      .left
      .value shouldBe Forge.NoRewardsAddressSpecified

  }

  property("fail to create if there is a LeaderElection failure") {
    forAll(blockCurve25519Gen.map(_.copy(timestamp = 1))) { parentBlock =>
      implicit val timeProvider: TimeProvider = mock[TimeProvider]
      val nodeView = ReadableNodeView(
        mock[HistoryReader[Block, BifrostSyncInfo]],
        mock[StateReader[ProgramId, Address]],
        mock[MemPoolReader[Transaction.TX]]
      )

      (nodeView.memPool
        .take[Int128](_: Int)(_: UnconfirmedTx[Transaction.TX] => Int128)(_: Ordering[Int128]))
        .expects(100, *, *)
        .once()
        .returning(Nil)

      (() => timeProvider.time)
        .expects()
        .once()
        // Low time delta will cause a LeaderElection failure
        .returning(1)

      (() => nodeView.history.bestBlock)
        .expects()
        .once()
        .returning(parentBlock)

      (nodeView.history
        .getTimestampsFrom(_: Block, _: Long))
        .expects(parentBlock, 3)
        .returning(Vector(parentBlock.timestamp))

      (nodeView.history
        .consensusStateAt(_: ModifierId))
        .expects(parentBlock.id)
        .returning(Right(NxtConsensus.State(10000000L, 0)))

      val rewardsAddress = keyRingCurve25519.addresses.head

      (nodeView.state
        .getTokenBoxes(_: Address))
        .expects(*)
        .anyNumberOfTimes()
        .onCall((a: Address) => Some(List(ArbitBox(a.evidence, nonce = Long.MaxValue, value = SimpleValue(1)))))

      val keyView =
        KeyView(
          keyRingCurve25519.addresses,
          Some(rewardsAddress),
          keyRingCurve25519.signWithAddress,
          keyRingCurve25519.lookupPublicKey
        )

      Forge
        .prepareForge(
          nodeView,
          keyView,
          0
        )
        .left
        .value shouldBe Forge.LeaderElectionFailure(NxtLeaderElection.NoBoxesEligible)
    }
  }

  override def beforeAll(): Unit =
    super.beforeAll()

}
