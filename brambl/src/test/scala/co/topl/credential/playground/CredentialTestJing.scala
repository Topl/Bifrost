package co.topl.credential.playground

import cats.data.NonEmptyChain
import cats.effect.unsafe.implicits.global
import co.topl.credential.Credential
import co.topl.credential.implicits._
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
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

import scala.collection.immutable.ListMap
import scala.util.Random

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

  val recipientAddress: DionAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.dionAddress

  val unprovenTransaction: Transaction.Unproven = Transaction.Unproven(
    inputs = List((combinedProp.dionAddress, Random.nextLong())),
    feeOutput = None,
    coinOutputs = NonEmptyChain(Transaction.PolyOutput(recipientAddress, Sized.maxUnsafe(BigInt(5)))),
    fee = Sized.maxUnsafe(BigInt(1)),
    timestamp = System.currentTimeMillis(),
    data = None,
    minting = false
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

  val transactionAll = Transaction(
    inputs = ListMap.empty[BoxReference, (Proposition, Proof)] ++ unprovenTransaction.inputs.map(
      _ -> (combinedProp, credential.proof)
    ),
    feeOutput = unprovenTransaction.feeOutput,
    coinOutputs = unprovenTransaction.coinOutputs,
    fee = unprovenTransaction.fee,
    timestamp = unprovenTransaction.timestamp,
    data = unprovenTransaction.data,
    minting = unprovenTransaction.minting
  )
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

  val transactionAdmins = Transaction(
    inputs = ListMap.empty[BoxReference, (Proposition, Proof)] ++ unprovenTransaction.inputs.map(
      _ -> (adminsProp, adminsProof)
    ),
    feeOutput = unprovenTransaction.feeOutput,
    coinOutputs = unprovenTransaction.coinOutputs,
    fee = unprovenTransaction.fee,
    timestamp = unprovenTransaction.timestamp,
    data = unprovenTransaction.data,
    minting = unprovenTransaction.minting
  )
  println(s"admin transaction: $transactionAdmins")
  println(verify(transactionAdmins, adminsProp, adminsProof, 2L))

  val transactionVoter1and2 = Transaction(
    inputs = ListMap.empty[BoxReference, (Proposition, Proof)] ++ unprovenTransaction.inputs.map(
      _ -> (votersThresholdProp, voter1and2ThresholdProof)
    ),
    feeOutput = unprovenTransaction.feeOutput,
    coinOutputs = unprovenTransaction.coinOutputs,
    fee = unprovenTransaction.fee,
    timestamp = unprovenTransaction.timestamp,
    data = unprovenTransaction.data,
    minting = unprovenTransaction.minting
  )
  println(s"voter transaction: $transactionVoter1and2")
  println(verify(transactionVoter1and2, votersThresholdProp, voter1and2ThresholdProof, 2L))

  def verify(transaction: Transaction, prop: Proposition, proof: Proof, height: Long): Boolean = {
    implicit val verificationContext: VerificationContext[F] =
      new VerificationContext[F] {
        def currentTransaction: Transaction = transaction
        def currentHeight: Long = height
        def inputBoxes: List[Box[Box.Value]] = List()
        def currentSlot: Slot = 1
      }
    prop.isSatisfiedBy[F](proof).unsafeRunSync()
  }
}
