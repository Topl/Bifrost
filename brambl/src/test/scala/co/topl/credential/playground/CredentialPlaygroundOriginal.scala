package co.topl.credential.playground

import cats.data.Chain
import cats.effect.unsafe.implicits.global
import co.topl.credential.Credential
import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.crypto.generation.KeyInitializer.Instances.{curve25519Initializer, ed25519Initializer}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.scripting.GraalVMScripting
import co.topl.scripting.GraalVMScripting.GraalVMValuable
import co.topl.scripting.GraalVMScripting.instances._
import co.topl.typeclasses.implicits._
import co.topl.typeclasses.VerificationContext
import io.circe.Json
import org.graalvm.polyglot.Value
import ModelGenerators._
import co.topl.crypto.generation.KeyInitializer

object CredentialPlaygroundOriginal extends App {
  type F[A] = cats.effect.IO[A]

  implicit val curve25519: Curve25519 = new Curve25519
  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = new ExtendedEd25519

  implicit val jsExecutor: Propositions.Script.JS.JSScript => F[(Json, Json) => F[Boolean]] =
    s =>
      GraalVMScripting
        .jsExecutor[F, Boolean](s.value)
        .map(f =>
          Function.untupled(
            f.compose[(Json, Json)] { t =>
              Seq(
                GraalVMValuable[Json].toGraalValue(t._1),
                GraalVMValuable[Json].toGraalValue(t._2),
                Value.asValue(new VerificationUtils)
              )
            }
          )
        )

  implicit val networkPrefix: NetworkPrefix = NetworkPrefix(1: Byte)

  // Exercise: Construct complex propositions and attempt to prove them using Credentials

  // Example:

  val stakingAddress: StakingAddress =
    StakingAddresses.Operator(ed25519.getVerificationKey(KeyInitializer[SecretKeys.Ed25519].random()))

  val offlineWalletSK =
    KeyInitializer[SecretKeys.Ed25519].random()

  def fullAddress(spendingAddress: SpendingAddress) = FullAddress(
    networkPrefix,
    spendingAddress,
    stakingAddress,
    ed25519.sign(offlineWalletSK, (spendingAddress, stakingAddress).signableBytes)
  )

  val party1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val party2SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()

  val party3Address: SpendingAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.spendingAddress

  val proposition = party1SK.vk.asProposition.and(party2SK.vk.asProposition)
  println(proposition)

  val unprovenTransaction: Transaction.Unproven =
    ModelGenerators.arbitraryUnprovenTransaction.arbitrary.first.copy(
      inputs = Chain(arbitraryTransactionUnprovenInput.arbitrary.first.copy(proposition = proposition)),
      outputs = Chain(
        Transaction.Output(fullAddress(party3Address), Box.Values.Poly(Sized.maxUnsafe(BigInt(10))), minting = false)
      )
    )

  val credential = Credential.Compositional.And(
    proposition,
    List(
      Credential.Knowledge.Ed25519(party1SK, unprovenTransaction),
      Credential.Knowledge.Curve25519(party2SK, unprovenTransaction)
    )
  )

  val proof = credential.proof
  println(proof)

  val transaction = unprovenTransaction.prove(_ => proof)
  println(transaction)

  implicit val verificationContext: VerificationContext[F] =
    new VerificationContext[F] {
      def currentTransaction: Transaction = transaction

      def currentHeight: Long = 50L
      def inputBoxes: List[Box] = List()
      def currentSlot: Slot = 1
    }

  val verificationResult: Boolean =
    proposition.isSatisfiedBy[F](proof).unsafeRunSync()
  println(verificationResult)

}
