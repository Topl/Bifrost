package co.topl.fullnode

import cats.data.{OptionT, StateT}
import cats.implicits._
import cats.{Id, _}
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.ConsensusValidation.Eval
import co.topl.consensus.{ConsensusValidation, LeaderElection, VrfRelativeStakeLookupAlgebra}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.crypto.typeclasses.ContainsVerificationKey.instances.ed25519ContainsVerificationKey
import co.topl.crypto.typeclasses.KeyInitializer.Instances._
import co.topl.crypto.typeclasses.{ContainsVerificationKey, KeyInitializer}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.typeclasses.BlockGenesis

object FullNode extends App {

  val stakerRelativeStake =
    Ratio(1, 5)

  def leaderElection[F[_]: Monad]: LeaderElection[F] = LeaderElection.Eval.make(
    Vrf
      .Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))
  )

  implicit val clock: ClockAlgebra[Id] = new SyncClockInterpreter

  private val Right(stakerAddress: TaktikosAddress) = {
    val stakingKey = KeyInitializer[PrivateKeys.Ed25519].random()
    val stakingVerificationKey =
      implicitly[ContainsVerificationKey[PrivateKeys.Ed25519, PublicKeys.Ed25519]].verificationKeyOf(stakingKey)
    val paymentKey = KeyInitializer[PrivateKeys.Ed25519].random()
    val paymentVerificationKey =
      implicitly[ContainsVerificationKey[PrivateKeys.Ed25519, PublicKeys.Ed25519]].verificationKeyOf(paymentKey)
    for {
      paymentVerificationKeyHash <- Sized.strict[Bytes, Lengths.`32`.type](
        Bytes(blake2b256.hash(paymentVerificationKey.bytes.data.toArray).value)
      )
      signature <- Sized.strict[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(1)))
    } yield TaktikosAddress(paymentVerificationKeyHash, stakingVerificationKey.bytes, signature)
  }

  val staker =
    Staker.Eval.make[Id](stakerAddress, clock, leaderElection)

  val initialState =
    InMemoryState(
      BlockGenesis(Nil).value,
      Map.empty,
      Map.empty,
      Map(0L -> Map(stakerAddress -> stakerRelativeStake)),
      Map(0L -> Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))))
    )

  private def consensusValidationEval(state: InMemoryState) =
    ConsensusValidation.Eval
      .make[Either[ConsensusValidation.Eval.Failure, *]](
        header =>
          clock
            .epochOf(header.slot)
            .map(state.epochNonce)
            .nonEmptyTraverse(Right(_): Either[ConsensusValidation.Eval.Failure, Eta]),
        new VrfRelativeStakeLookupAlgebra[Either[ConsensusValidation.Eval.Failure, *]] {

          def lookupAt(
            block:   BlockHeaderV2
          )(address: TaktikosAddress): Either[ConsensusValidation.Eval.Failure, Ratio] =
            OptionT(clock.epochOf(block.slot).map(state.relativeStakes(_).get(address)))
              .getOrElse(Ratio(0))
              .asRight
        },
        leaderElection[Either[ConsensusValidation.Eval.Failure, *]]
      )

  val stateT =
    StateT[Id, InMemoryState, Option[BlockV2]] { implicit state =>
      staker
        .mintBlock(
          state.canonicalHead,
          transactions = Nil,
          header => address => clock.epochOf(header.slot).map(state.relativeStakes.get(_).flatMap(_.get(address))),
          header => clock.epochOf(header.slot).map(state.epochNonce)
        )
        .fmap {
          case Some(newBlock: BlockV2) =>
            consensusValidationEval(state)
              .validate(newBlock.headerV2, state.canonicalHead.headerV2)
              .handleError(f => throw new Exception(f.toString))

            state.append(newBlock) -> Some(newBlock)
          case _ =>
            state -> None
        }
    }

  LazyList
    .unfold(initialState)(stateT.run(_).swap.some)
    .takeWhile(_.nonEmpty)
    .collect { case Some(newBlock) => newBlock }
    .foreach { head =>
      import co.topl.typeclasses.ShowInstances._
      println(s"Applied ${head.headerV2.show}")
    }

  println("Completed epoch")

}
