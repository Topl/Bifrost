package co.topl.demo

import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.consensus._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LeaderElectionValidationAlgebra}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.typeclasses._
import co.topl.crypto.typeclasses.implicits._
import co.topl.minting._
import co.topl.minting.algebras.{BlockMintAlgebra, LeaderElectionMintingAlgebra}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._

import scala.concurrent.duration._

object TetraDemo extends App {

  type F[A] = Either[BlockHeaderValidation.Failure, A]

  val stakerRelativeStake =
    Ratio(1, 2)

  val leaderElectionThreshold: LeaderElectionValidationAlgebra[F] =
    LeaderElectionValidation.Eval.make(
      Vrf
        .Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))
    )

  private val stakerVrfKey =
    KeyInitializer[SecretKeys.Vrf].random()

  val stakerRegistration: Box.Values.TaktikosRegistration =
    Box.Values.TaktikosRegistration(
      vrfCommitment = stakerVrfKey.ed25519
        .prove[Proofs.Signature.Ed25519, VerificationKeys.Vrf],
      extendedVk = ???,
      registrationSlot = 0
    )

  val leaderElectionHit: LeaderElectionMintingAlgebra[F] = LeaderElectionMinting.Eval.make(
    stakerVrfKey,
    leaderElectionThreshold
  )

  val clock: ClockAlgebra[F] =
    new SyncClockInterpreter[F](100.milli, 150)

  private val stakerAddress: TaktikosAddress = {
    val stakingKey = KeyInitializer[SecretKeys.Ed25519].random()
    val stakingVerificationKey =
      implicitly[ContainsVerificationKey[SecretKeys.Ed25519, VerificationKeys.Ed25519]].verificationKeyOf(stakingKey)
    val paymentKey = KeyInitializer[SecretKeys.Ed25519].random()
    val paymentVerificationKey =
      implicitly[ContainsVerificationKey[SecretKeys.Ed25519, VerificationKeys.Ed25519]].verificationKeyOf(paymentKey)
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
        .tabulate(10)(idx => idx.toLong -> Map(stakerAddress -> stakerRegistration))
        .toMap,
      List
        .tabulate(10)(idx => idx.toLong -> Sized.strictUnsafe[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(0))))
        .toMap
    )

  val mint: BlockMintAlgebra[F] =
    BlockMint.Eval.make(
      Staking.Eval.make(
        stakerAddress,
        leaderElectionHit,
        BlockSigning.Eval.make(
          clock,
          KeyEvolver.InMemory.make {
            implicit val slot: Slot = 0
            KeyInitializer[SecretKeys.SymmetricMMM].random()
          }
        ),
        (currentSlot: Slot, address: TaktikosAddress) =>
          clock.epochOf(currentSlot).map(epoch => inMemoryState.relativeStakes(epoch).get(address)),
        (slot: Slot) => clock.epochOf(slot).map(inMemoryState.epochNonce)
      ),
      clock
    )

  val consensusValidation: BlockHeaderValidationAlgebra[F] =
    BlockHeaderValidation.Eval.Stateful
      .make(
        slotId => clock.epochOf(slotId._1).map(inMemoryState.epochNonce),
        (slotId, address) => clock.epochOf(slotId._1).map(epoch => inMemoryState.relativeStakes(epoch).get(address)),
        leaderElectionThreshold,
        (slotId, address) => clock.epochOf(slotId._1).map(epoch => inMemoryState.registrations(epoch).get(address))
      )

  for {
    initialSlot <- clock.currentSlot()
    _           <- clock.delayedUntilSlot(initialSlot + 1)
    currentSlot <- clock.currentSlot()
    _ <- currentSlot.iterateWhileM(slot =>
      // Mint a new block
      mint
        .mint(inMemoryState.canonicalHead.headerV2, Nil, slot)
        .map(
          _.foreach { nextBlock =>
            // If a block was minted at this slot, attempt to validate it
            consensusValidation
              .validate(nextBlock.headerV2, inMemoryState.canonicalHead.headerV2)
              .onError { case e =>
                throw new IllegalArgumentException(e.toString)
              }
            // If valid, append the block and log it
            inMemoryState = inMemoryState.append(nextBlock)
            println(s"Appended block ${nextBlock.headerV2.show}")
          }
        )
        .flatMap(_ =>
          // Log potential epoch change
          clock
            .epochOf(slot)
            .flatMap(epoch =>
              clock
                .epochRange(epoch)
                .map(boundary =>
                  if ((boundary.start: Slot) == slot) {
                    println(s"Starting epoch=$epoch")
                  } else {}
                )
            )
        )
        // Delay until the next slot
        .flatMap(_ => clock.delayedUntilSlot(slot + 1))
        .map(_ => slot + 1)
    )(slot => slot <= 600)
  } yield ()

}
