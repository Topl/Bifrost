package co.topl.credential

import cats.Id
import cats.data.NonEmptyChain
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._
import co.topl.typeclasses.{KeyInitializer, VerificationContext}

import scala.collection.immutable.ListMap
import scala.util.Random

object CredentialPlayground extends App {
  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = ExtendedEd25519.precomputed()
  implicit val networkPrefix: NetworkPrefix = NetworkPrefix(1: Byte)

  // Exercise: Construct complex propositions and attempt to prove them using Credentials

  // Example:
  val party1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val party2SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()

  val party3Address: DionAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.dionAddress

  val proposition = party1SK.vk.asProposition.and(party2SK.vk.asProposition)
  println(proposition)

  val unprovenTransaction: Transaction.Unproven = Transaction.Unproven(
    inputs = List((proposition.dionAddress, Random.nextLong())),
    feeOutput = None,
    coinOutputs = NonEmptyChain(Transaction.PolyOutput(party3Address, Sized.maxUnsafe(BigInt(10)))),
    fee = Sized.maxUnsafe(BigInt(5)),
    timestamp = System.currentTimeMillis(),
    data = None,
    minting = false
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

  val transaction = Transaction(
    inputs = ListMap.from(unprovenTransaction.inputs.map(_ -> (proposition, proof))),
    feeOutput = unprovenTransaction.feeOutput,
    coinOutputs = unprovenTransaction.coinOutputs,
    fee = unprovenTransaction.fee,
    timestamp = unprovenTransaction.timestamp,
    data = unprovenTransaction.data,
    minting = unprovenTransaction.minting
  )
  println(transaction)

}


/**
 * 1. Should add a contextual proof to check the output box of the current transaction
 * 2. Would be interested to explore how to "group" transactions into a "context" and evaluate a series of transactions with their intermediary state
 * 3.
 */
object JamesRequiredOutput extends App {
  // secret sharing structure (one of one)
  // HTLC
  // token excahnge with buyback
  // ICO
  // rock-paper-scissors
  //

  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = ExtendedEd25519.precomputed()
  implicit val networkPrefix: NetworkPrefix = NetworkPrefix(1: Byte)

  // Exercise: Construct complex propositions and attempt to prove them using Credentials

  // Example:
  val party1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val party2SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()

  val party3Address: DionAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.dionAddress

  // Locking proposition for the initial transaction
  val proposition = party1SK.vk.asProposition.and(Propositions.Contextual.RequiredDionOutput(0, party3Address))
  println(proposition.dionAddress)

  val unprovenTransaction: Transaction.Unproven = Transaction.Unproven(
    inputs = List((proposition.dionAddress, Random.nextLong())),
    feeOutput = None,
    coinOutputs = NonEmptyChain(Transaction.PolyOutput(party3Address, Sized.maxUnsafe(BigInt(10)))),
    fee = Sized.maxUnsafe(BigInt(5)),
    timestamp = System.currentTimeMillis(),
    data = None,
    minting = false
  )

  val credential = Credential.Compositional.And(
    proposition,
    List(
      Credential.Knowledge.Ed25519(party1SK, unprovenTransaction),
      Credential.Contextual.RequiredDionOutput(0, party3Address)
    )
  )

  val proof = credential.proof
  println(proof)

  val transaction = Transaction(
    inputs = ListMap.from(unprovenTransaction.inputs.map(_ -> (proposition, proof))),
    feeOutput = unprovenTransaction.feeOutput,
    coinOutputs = unprovenTransaction.coinOutputs,
    fee = unprovenTransaction.fee,
    timestamp = unprovenTransaction.timestamp,
    data = unprovenTransaction.data,
    minting = unprovenTransaction.minting
  )
  println(transaction)

  implicit val context: VerificationContext[Id] = new VerificationContext[Id] {
    def currentTransaction: Transaction = transaction
    def currentHeight: Long = 1
  }

  println(s"Does the proof satisfy the proposition? ${proposition isSatisfiedBy proof}")


}
