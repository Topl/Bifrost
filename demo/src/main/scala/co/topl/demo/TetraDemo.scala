package co.topl.demo

import cats._
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.consensus.{ConsensusValidation, LeaderElection}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.typeclasses.implicits._
import co.topl.crypto.typeclasses.{ContainsVerificationKey, KeyInitializer}
import co.topl.minting.{BlockMint, BlockSigning, KeyEvolver, Staking}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.typeclasses.BlockGenesis
import co.topl.typeclasses.implicits._

import scala.concurrent.duration._

object TetraDemo extends App {

  type F[A] = Either[ConsensusValidation.Eval.Failure, A]

  val stakerRelativeStake =
    Ratio(1, 5)

  def leaderElectionThreshold[F[_]: Monad]: LeaderElectionEligibilityAlgebra[F] =
    LeaderElection.Threshold.Eval.make(
      Vrf
        .Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))
    )

  private val stakerVrfKey = KeyInitializer[PrivateKeys.Vrf].random()

  val leaderElectionHit: LeaderElectionHitAlgebra[F] = LeaderElection.Hit.Eval.make(
    stakerVrfKey,
    leaderElectionThreshold
  )

  val clock: ClockAlgebra[F] =
    new SyncClockInterpreter[F](10.milli, 600)

  private val stakerAddress: TaktikosAddress = {
    val stakingKey = KeyInitializer[PrivateKeys.Ed25519].random()
    val stakingVerificationKey =
      implicitly[ContainsVerificationKey[PrivateKeys.Ed25519, PublicKeys.Ed25519]].verificationKeyOf(stakingKey)
    val paymentKey = KeyInitializer[PrivateKeys.Ed25519].random()
    val paymentVerificationKey =
      implicitly[ContainsVerificationKey[PrivateKeys.Ed25519, PublicKeys.Ed25519]].verificationKeyOf(paymentKey)
    TaktikosAddress(
      Sized.strictUnsafe(
        Bytes(blake2b256.hash(paymentVerificationKey.bytes.data.toArray).value)
      ),
      stakingVerificationKey.bytes,
      Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(1)))
    )
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

  val relativeStakeLookup: VrfRelativeStakeLookupAlgebra[F] =
    (block: BlockHeaderV2, currentSlot: Slot, address: TaktikosAddress) =>
      clock.epochOf(block.slot).map(epoch => inMemoryState.relativeStakes(epoch).get(address))

  val etaLookup: EtaLookupAlgebra[F] =
    (block: BlockHeaderV2, currentSlot: Slot) =>
      clock
        .epochOf(currentSlot)
        .map(inMemoryState.epochNonce)

  val mint: BlockMintAlgebra[F] =
    BlockMint.Eval.make(
      Staking.Eval.make(
        stakerAddress,
        leaderElectionHit,
        BlockSigning.Eval.make(
          clock,
          KeyEvolver.InMemory.make {
            implicit val slot: Slot = 0
            KeyInitializer[PrivateKeys.Kes].random()
          }
        ),
        relativeStakeLookup,
        etaLookup
      ),
      clock
    )

  val consensusValidation: BlockHeaderValidationAlgebra[F] =
    ConsensusValidation.Eval
      .make(
        etaLookup,
        relativeStakeLookup,
        leaderElectionThreshold
      )

  clock
    .currentSlot()
    .flatMap(currentSlot => clock.delayedUntilSlot(currentSlot + 1))
    .flatMap(_ =>
      clock
        .currentSlot()
        .map(
          _.iterateWhileM(s =>
            // Mint a new block
            mint
              .mint(inMemoryState.canonicalHead.headerV2, Nil, s)
              .map(
                _.foreach { nextBlock =>
                  // If a block was minted at this slot, attempt to validate it
                  consensusValidation
                    .validate(nextBlock.headerV2, inMemoryState.canonicalHead.headerV2)
                    .recover { case e =>
                      throw new Exception(e.toString)
                    }
                  // If valid, append the block and log it
                  inMemoryState = inMemoryState.append(nextBlock)
                  println(s"Appended block ${nextBlock.headerV2.show}")
                }
              )
              .flatMap(_ =>
                // Log potential epoch change
                clock
                  .epochOf(s)
                  .flatMap(epoch =>
                    clock
                      .epochBoundary(epoch)
                      .map(boundary =>
                        if ((boundary.start: Slot) == s) {
                          println(s"Starting epoch=$epoch")
                        } else {}
                      )
                  )
              )
              // Delay until the next slot
              .flatMap(_ => clock.delayedUntilSlot(s + 1))
              .map(_ => s + 1)
          )(_ <= 2400)
        )
    )

}
