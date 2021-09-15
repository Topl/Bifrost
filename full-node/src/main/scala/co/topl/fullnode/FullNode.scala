package co.topl.fullnode

import cats.Id
import cats.data.{OptionT, StateT}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.{ConsensusValidationProgram, LeaderElection, VrfRelativeStakeLookupAlgebra}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.typeclasses.BlockGenesis
import co.topl.typeclasses.crypto.ContainsVerificationKey.instances.ed25519ContainsVerificationKey
import co.topl.typeclasses.crypto.KeyInitializer.Instances._
import co.topl.typeclasses.crypto.{ContainsVerificationKey, KeyInitializer}
import co.topl.typeclasses.ShowInstances._

object FullNode extends App {

  val stakerRelativeStake =
    Ratio(1, 5)

  implicit val vrfConfig: Vrf.Config =
    Vrf
      .Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))

  implicit val vrf: Ed25519VRF = new Ed25519VRF
  vrf.precompute()

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

  val staker = Staker(stakerAddress)

  val initialState =
    InMemoryState(
      BlockGenesis(Nil).value,
      Map.empty,
      Map.empty,
      Map(0L -> Map(stakerAddress -> stakerRelativeStake)),
      Map(0L -> Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))))
    )

  val stateT =
    StateT[Id, InMemoryState, Option[BlockV2]] { implicit state =>
      staker
        .mintBlock(
          state.canonicalHead,
          transactions = Nil,
          header => address => state.relativeStakes.get(clock.epochOf(header.slot)).flatMap(_.get(address)),
          header => state.epochNonce(clock.epochOf(header.slot))
        )
        .value match {
        case Some(newBlock) =>
          new ConsensusValidationProgram[Id](
            header => state.epochNonce(clock.epochOf(header.slot)),
            new VrfRelativeStakeLookupAlgebra[Id] {

              def lookupAt(block: BlockHeaderV2)(address: TaktikosAddress): OptionT[Id, Ratio] =
                OptionT.fromOption(state.relativeStakes(clock.epochOf(block.slot)).get(address))
            },
            clock
          ).validate(newBlock.headerV2, state.canonicalHead.headerV2)
            .valueOr(f => throw new Exception(f.toString))

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
      println(show"Applied ${head.headerV2}")
    }

  println("Completed epoch")

}
