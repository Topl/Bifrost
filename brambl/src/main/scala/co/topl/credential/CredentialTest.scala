package co.topl.credential

import cats.data.NonEmptyChain
import co.topl.credential.CredentialPlayground.F
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._
import co.topl.typeclasses.{KeyInitializer, VerificationContext}
import co.topl.credential.implicits._

import scala.collection.immutable.ListMap
import scala.util.Random

object CredentialTest extends App {
  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = ExtendedEd25519.precomputed()
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

  val credential = (combinedProp,
    Iterable(
      Credential.Knowledge.Ed25519(voter1SK, unprovenTransaction),
      Credential.Knowledge.Ed25519(voter2SK, unprovenTransaction),
      Credential.Knowledge.Curve25519(voter3SK, unprovenTransaction),
      heightProp.toCredential,
      Credential.Knowledge.Ed25519(admin1SK, unprovenTransaction),
      Credential.Knowledge.Curve25519(admin2SK, unprovenTransaction)
    )).toCredential

  val transactionAll = Transaction(
    inputs = ListMap.from(unprovenTransaction.inputs.map(_ -> (combinedProp, credential.proof))),
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
    inputs = ListMap.from(unprovenTransaction.inputs.map(_ -> (adminsProp, adminsProof))),
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
    inputs = ListMap.from(unprovenTransaction.inputs.map(_ -> (votersThresholdProp, voter1and2ThresholdProof))),
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
      }
    prop.isSatisifiedBy[F](proof)
  }
}
