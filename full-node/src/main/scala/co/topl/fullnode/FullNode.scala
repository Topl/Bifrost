package co.topl.fullnode

import cats.Id
import cats.data.{NonEmptyChain, OptionT, StateT}
import cats.implicits._
import co.topl.algebras.Clock
import co.topl.consensus.{ConsensusValidationProgram, LeaderElection, RelativeStateLookupAlgebra}
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.typeclasses.BlockGenesis
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

object FullNode extends App {

  val stakerRelativeStake =
    Ratio(1, 5)

  implicit val leaderElectionConfig: LeaderElection.Config =
    LeaderElection
      .Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))

  implicit val vrf: Ed25519VRF = new Ed25519VRF
  vrf.precompute()

  implicit val clock: Clock[Id] = new SyncClock

  private val Right(stakerAddress: TaktikosAddress) =
    for {
      paymentVerificationKeyHash <- Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
      stakingVerificationKey     <- Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
      signature                  <- Sized.strict[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(1)))
    } yield TaktikosAddress(paymentVerificationKeyHash, stakingVerificationKey, signature)

  val staker = Staker(stakerAddress)

  val initialState =
    InMemoryState(
      NonEmptyChain(Tine(NonEmptyChain(BlockGenesis(Nil).value))),
      Map(stakerAddress -> stakerRelativeStake),
      Bytes(Array.fill[Byte](4)(0))
    )

  val stateT =
    StateT[Id, InMemoryState, BlockV2] { implicit state =>
      val newBlock =
        staker.mintBlock(state.head, transactions = Nil, state.relativeStake, state.epochNonce)

      new ConsensusValidationProgram[Id](
        _ => OptionT.pure[Id](state.epochNonce),
        new RelativeStateLookupAlgebra[Id] {
          def lookup(epoch: Epoch)(address: TaktikosAddress): OptionT[Id, Ratio] =
            OptionT.fromOption(state.relativeStake.get(address))
        },
        clock
      ).validate(newBlock.headerV2, state.head.headerV2)
        .valueOr(f => throw new Exception(f.toString))

      state.append(newBlock) -> newBlock
    }

  LazyList
    .unfold(initialState)(stateT.run(_).swap.some)
    .takeWhile(_.headerV2.slot < clock.slotsPerEpoch)
    .foreach { head =>
      println(
        s"Applied headerId=${new String(head.headerV2.id.dataBytes.toArray)}" +
        s" to parentHeaderId=${new String(head.headerV2.parentHeaderId.dataBytes.toArray)}" +
        s" at height=${head.headerV2.height}" +
        s" at slot=${head.headerV2.slot}" +
        s" at timestamp=${head.headerV2.timestamp}"
      )
    }

  println("Completed epoch")

}
