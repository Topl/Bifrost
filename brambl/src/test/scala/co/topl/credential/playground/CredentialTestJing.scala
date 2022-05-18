package co.topl.credential.playground

import cats.data.Chain
import cats.effect.unsafe.implicits.global
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.credential.Credential
import co.topl.credential.implicits._
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.scripting.GraalVMScripting
import co.topl.scripting.GraalVMScripting.GraalVMValuable
import co.topl.scripting.GraalVMScripting.instances._
import co.topl.typeclasses.implicits._
import co.topl.typeclasses.{KeyInitializer, VerificationContext}
import io.circe.Json
import org.graalvm.polyglot.Value

object CredentialTestJing extends App {
  type F[A] = cats.effect.IO[A]

  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = ExtendedEd25519.precomputed()

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

  // Exercise: Construct complex propositions and attempt to prove them using Credentials
  val admin1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val admin2SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()
  val voter1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val voter2SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val voter3SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()

  val admin1Prop = admin1SK.vk.asProposition
  val admin2Prop = admin2SK.vk.asProposition
  val voter1Prop = voter1SK.vk.asProposition
  val voter2Prop = voter2SK.vk.asProposition
  val voter3Prop = voter3SK.vk.asProposition

  val heightProp = Propositions.Contextual.HeightLock(3)
  val adminsProp = admin1Prop.and(admin2Prop)
  val votersThresholdProp = List(voter1Prop, voter2Prop, voter3Prop, heightProp).threshold(2)
  val combinedProp = votersThresholdProp or adminsProp

  val recipientAddress: SpendingAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.spendingAddress

  val unprovenTransaction: Transaction.Unproven =
    ModelGenerators.arbitraryUnprovenTransaction.arbitrary.first.copy(
      inputs = Chain(arbitraryTransactionUnprovenInput.arbitrary.first.copy(proposition = combinedProp)),
      outputs = Chain(
        Transaction.Output(fullAddress(recipientAddress), Box.Values.Poly(Sized.maxUnsafe(BigInt(5))), minting = false)
      )
    )

  val credential = (
    combinedProp,
    Iterable(
      Credential.Knowledge.Ed25519(voter1SK, unprovenTransaction),
      Credential.Knowledge.Ed25519(voter2SK, unprovenTransaction),
      Credential.Knowledge.Curve25519(voter3SK, unprovenTransaction),
      heightProp.toCredential,
      Credential.Knowledge.Ed25519(admin1SK, unprovenTransaction),
      Credential.Knowledge.Curve25519(admin2SK, unprovenTransaction)
    )
  ).toCredential

  val transactionAll = unprovenTransaction.prove(_ => credential.proof)
  println(s"all transaction: $transactionAll")
  println(verify(transactionAll, combinedProp, credential.proof, 2L))

  val adminsCredential = Credential.Compositional.And(
    adminsProp,
    List(
      Credential.Knowledge.Ed25519(admin1SK, unprovenTransaction),
      Credential.Knowledge.Curve25519(admin2SK, unprovenTransaction)
    )
  )
  val adminsProof = adminsCredential.proof
//    println(adminsProof)

  val voter1and2ThresholdCredential =
    Credential.Compositional.Threshold(
      votersThresholdProp,
      List(
        Credential.Knowledge.Ed25519(voter1SK, unprovenTransaction),
        Credential.Knowledge.Ed25519(voter2SK, unprovenTransaction)
      )
    )
  val voter1and2ThresholdProof = voter1and2ThresholdCredential.proof
//    println(voter1and2ThresholdProof)

  val transactionAdmins = unprovenTransaction.prove(_ => adminsProof)
  println(s"admin transaction: $transactionAdmins")
  println(verify(transactionAdmins, adminsProp, adminsProof, 2L))

  val transactionVoter1and2 = unprovenTransaction.prove(_ => voter1and2ThresholdProof)
  println(s"voter transaction: $transactionVoter1and2")
  println(verify(transactionVoter1and2, votersThresholdProp, voter1and2ThresholdProof, 2L))

  def verify(transaction: Transaction, prop: Proposition, proof: Proof, height: Long): Boolean = {
    implicit val verificationContext: VerificationContext[F] =
      new VerificationContext[F] {
        def currentTransaction: Transaction = transaction
        def currentHeight: Long = height
        def inputBoxes: List[Box] = List()
        def currentSlot: Slot = 1
      }
    prop.isSatisfiedBy[F](proof).unsafeRunSync()
  }
}
