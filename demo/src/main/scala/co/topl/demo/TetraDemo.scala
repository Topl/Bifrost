package co.topl.demo

import cats._
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.consensus.{ConsensusValidation, LeaderElection}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.typeclasses.implicits._
import co.topl.crypto.typeclasses.{ContainsVerificationKey, KeyInitializer}
import co.topl.minting.{BlockMint, BlockSigning, Staker}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.typeclasses.BlockGenesis
import co.topl.typeclasses.implicits._

import scala.concurrent.duration._

object TetraDemo extends App {

  val stakerRelativeStake =
    Ratio(1, 5)

  def leaderElectionThreshold[F[_]: Monad]: LeaderElectionThresholdAlgebra[F] =
    LeaderElection.Threshold.Eval.make(
      Vrf
        .Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))
    )

  private val stakerVrfKey = KeyInitializer[PrivateKeys.Vrf].random()

  def leaderElectionHit[F[_]: Monad]: LeaderElectionHitAlgebra[F] = LeaderElection.Hit.Eval.make(
    stakerVrfKey,
    leaderElectionThreshold
  )

  def clock[F[_]: Applicative]: ClockAlgebra[F] = new SyncClockInterpreter(10.milli, 600)

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

  private var inMemoryState: InMemoryState =
    InMemoryState(
      BlockGenesis(Nil).value,
      Map.empty,
      Map.empty,
      List
        .tabulate(10)(idx => idx.toLong -> Map(stakerAddress -> stakerRelativeStake))
        .toMap,
      List
        .tabulate(10)(idx => idx.toLong -> Sized.strictUnsafe[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(0))))
        .toMap
    )

  def relativeStakeLookup[F[_]: Applicative]: VrfRelativeStakeLookupAlgebra[F] =
    (block: BlockHeaderV2, currentSlot: Slot, address: TaktikosAddress) =>
      clock[F].epochOf(block.slot).map(inMemoryState.relativeStakes(_).get(address))

  def etaLookup[F[_]: Applicative]: EtaLookupAlgebra[F] =
    (block: BlockHeaderV2, currentSlot: Slot) =>
      clock[F]
        .epochOf(currentSlot)
        .map(inMemoryState.epochNonce)

  val staker: Staker[Id] = {
    val evolver: KeyEvolverAlgebra[Id] =
      new KeyEvolverAlgebra[Id] {

        private var key = {
          implicit val slot: Slot = 0
          KeyInitializer[PrivateKeys.Kes].random()
        }

        def evolvedKey(slot: Slot): Id[PrivateKeys.Kes] = {
          key = key.evolveSteps(slot)
          key.pure[Id]
        }
      }
    Staker.Eval.make[Id](
      stakerAddress,
      clock,
      leaderElectionHit,
      BlockMint.Eval.make(stakerAddress),
      BlockSigning.Eval.make(clock, evolver),
      relativeStakeLookup,
      etaLookup
    )
  }

  val consensusValidation =
    ConsensusValidation.Eval
      .make[Either[ConsensusValidation.Eval.Failure, *]](
        etaLookup,
        relativeStakeLookup,
        leaderElectionThreshold
      )

  val c = clock[Id]
  c.delayedUntilSlot(c.currentSlot() + 1)
  var s = c.currentSlot()
  while (s <= 2400) {
    staker
      .mintBlock(inMemoryState.canonicalHead, Nil, s)
      .foreach { nextBlock =>
        consensusValidation
          .validate(nextBlock.headerV2, inMemoryState.canonicalHead.headerV2)
          .recover { case e =>
            throw new Exception(e.toString)
          }
        inMemoryState = inMemoryState.append(nextBlock)
        println(s"Appended block ${nextBlock.headerV2.show}")
      }
    val epoch = c.epochOf(s)
    if (c.epochBoundary(epoch).start == s) {
      println(s"Starting epoch=$epoch")
    }
    c.delayedUntilSlot(s + 1)
    s = s + 1
  }

}
