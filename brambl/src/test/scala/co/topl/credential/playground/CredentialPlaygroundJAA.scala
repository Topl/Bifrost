package co.topl.credential.playground

import cats.data.NonEmptyChain
import cats.effect.unsafe.implicits.global
import co.topl.credential.Credential
import co.topl.crypto.hash.blake2b256
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

///**
// *  contextual proof to check the output box of the current transaction
// *  secret sharing structure (one of one)
// *  HTLC
// *  token excahnge with buyback
// *  ICO
// *  rock-paper-scissors
// *  how could I make our token ontology?
// */

object SetupSandbox {
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

  val curve25519Sk: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()
  val ed25519Sk: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val aliceSk: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()
  val bobSk: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()

  val address0: DionAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.dionAddress
  val address1: DionAddress = curve25519Sk.vk.asProposition.and(ed25519Sk.vk.asProposition).dionAddress

  def createUnprovenTransaction(
    inputs:      List[BoxReference],
    coinOutputs: NonEmptyChain[Transaction.CoinOutput]
  ): Transaction.Unproven =
    Transaction.Unproven(
      inputs,
      feeOutput = None,
      coinOutputs,
      fee = Sized.maxUnsafe(BigInt(5)),
      timestamp = System.currentTimeMillis(),
      data = None,
      minting = false
    )

  def transactionFromUnproven(unprovenTransaction: Transaction.Unproven, attestation: Attestation): Transaction =
    Transaction(
      inputs = ListMap.from(unprovenTransaction.inputs.zip(attestation)),
      feeOutput = unprovenTransaction.feeOutput,
      coinOutputs = unprovenTransaction.coinOutputs,
      fee = unprovenTransaction.fee,
      timestamp = unprovenTransaction.timestamp,
      data = unprovenTransaction.data,
      minting = unprovenTransaction.minting
    )
}

object CredentialPlaygroundJAA extends App {
  import SetupSandbox._

  val proposition = curve25519Sk.vk.asProposition.and(ed25519Sk.vk.asProposition)
  println(s"The address for the proposition is: ${proposition.dionAddress}")

  val unprovenTransaction: Transaction.Unproven = createUnprovenTransaction(
    List((proposition.dionAddress, Random.nextLong())),
    NonEmptyChain(Transaction.PolyOutput(address0, Sized.maxUnsafe(BigInt(10))))
  )

  val credential = Credential.Compositional.And(
    proposition,
    List(
      Credential.Knowledge.Ed25519(ed25519Sk, unprovenTransaction),
      Credential.Knowledge.Curve25519(curve25519Sk, unprovenTransaction)
    )
  )

  val proof = credential.proof
  println(proof)

  val transaction = transactionFromUnproven(unprovenTransaction, ListMap(proposition -> proof))
  println(transaction)

  implicit val context: VerificationContext[F] = new VerificationContext[F] {
    def currentTransaction: Transaction = transaction
    def currentHeight: Long = 1

    def inputBoxes: List[Box[Box.Value]] = List(
      Box(
        proposition.typedEvidence,
        unprovenTransaction.inputs.head._2,
        Box.Values.Poly(Sized.maxUnsafe(BigInt(10))),
        10
      )
    )

    def currentSlot: Slot = 1
  }

  println(s"Does the proof satisfy the proposition? ${proposition.isSatisfiedBy(proof).unsafeRunSync()}")

}

object RequiredOutput extends App {
  import SetupSandbox._

  val requiredBoxProposition = Propositions.Contextual.RequiredBoxState(
    BoxLocations.Output,
    List((0, Box.empty.copy(evidence = address0.typedEvidence))) // todo: helper function name for Box.empty.copy
  )
  val proposition = curve25519Sk.vk.asProposition.and(requiredBoxProposition)
  println(s"The address for the proposition is: ${proposition.dionAddress}")

  val unprovenTransaction: Transaction.Unproven = createUnprovenTransaction(
    List((proposition.dionAddress, Random.nextLong())),
    NonEmptyChain(Transaction.PolyOutput(address0, Sized.maxUnsafe(BigInt(10))))
  )

  val credential = Credential.Compositional.And(
    proposition,
    List(
      Credential.Knowledge.Curve25519(curve25519Sk, unprovenTransaction),
      Credential.Contextual
        .RequiredBoxState(BoxLocations.Output, List((0, Box.empty.copy(evidence = address0.typedEvidence))))
    )
  )

  val proof = credential.proof
  println(proof)

  val transaction = transactionFromUnproven(unprovenTransaction, ListMap(proposition -> proof))
  println(transaction)

  implicit val context: VerificationContext[F] = new VerificationContext[F] {
    def currentTransaction: Transaction = transaction
    def currentHeight: Long = 1

    def inputBoxes: List[Box[Box.Value]] = List(
      Box(
        proposition.typedEvidence,
        unprovenTransaction.inputs.head._2,
        Box.Values.Poly(Sized.maxUnsafe(BigInt(10))),
        10
      )
    )
    def currentSlot: Slot = 1
  }

  println(s"Does the proof satisfy the proposition? ${proposition.isSatisfiedBy(proof).unsafeRunSync()}")
}

//object XorGameSetup extends App {
//  import SetupSandbox._
//
//  val aliceSaltInput = blake2b256.hash("test".getBytes)
//  val aliceValueInput: Byte = 0
//  val aliceCommit: Digest32 = Sized.strictUnsafe(Bytes(blake2b256.hash(aliceSaltInput.value :+ aliceValueInput).value))
//
//  val requiredInputBoxProposition =
//    Propositions.Contextual.RequiredBoxState(BoxLocations.Input, List((0, Box.empty.copy(data = aliceValueInput))))
//
//  val fullGameProposition = bobSk.vk.asProposition
//    .and(Propositions.Contextual.HeightLock(50))
//    .or(
//      Propositions.Knowledge
//        .HashLock(aliceCommit)
//        .and(
//          aliceSk.vk.asProposition
//            .and(requiredInputBoxProposition)
//            .or(bobSk.vk.asProposition)
//        )
//    )
//
//  val requiredOutputBoxProposition = Propositions.Contextual.RequiredBoxState(
//    BoxLocations.Output,
//    List((0, Box.empty.copy(evidence = fullGameProposition.typedEvidence)))
//  )
//
//  val halfGameProposition = Propositions.Example
//    .EnumeratedInput(List(0, 1))
//    .and(requiredOutputBoxProposition)
//
//  println(s"The address for the proposition is: ${fullGameProposition.dionAddress}")
//
//  val unprovenSetupGame: Transaction.Unproven = createUnprovenTransaction(
//    List((address0, Random.nextLong())),
//    NonEmptyChain(Transaction.PolyOutput(fullGameProposition.dionAddress, Sized.maxUnsafe(BigInt(10))))
//  )
//
//  val halfGameCredential = Credential.Compositional.And(
//    halfGameProposition,
//    List(
//      Credential.Example.EnumeratedInput(List(0, 1), 0),
//      Credential.Contextual.RequiredBoxState(
//        BoxLocations.Output,
//        List((0, Box.empty.copy(evidence = fullGameProposition.dionAddress.typedEvidence)))
//      )
//    )
//  )
//
//  val halfGameProof = halfGameCredential.proof
//  println(halfGameProof)
//
//  val transaction = transactionFromUnproven(unprovenSetupGame, ListMap(halfGameProposition -> halfGameProof))
//  println(transaction)
//
//  implicit val context: VerificationContext[F] = new VerificationContext[F] {
//    def currentTransaction: Transaction = transaction
//    def currentHeight: Long = 1
//    def inputBoxes: List[Box[Box.Value]] = List()
//    def currentSlot: Slot = 1
//  }
//
//  println(
//    s"Does the proof satisfy the proposition? ${halfGameProposition.isSatisfiedBy(halfGameProof).unsafeRunSync()}"
//  )
//}

object XorGameCompletion extends App {
  import SetupSandbox._

  val aliceSaltInput = blake2b256.hash("test".getBytes)
  val aliceValueInput: Byte = 1
  val aliceCommit: Digest32 = Sized.strictUnsafe(Bytes(blake2b256.hash(aliceSaltInput.value :+ aliceValueInput).value))

  val requiredInputBoxProposition =
    Propositions.Contextual.RequiredBoxState(
      BoxLocations.Input,
      List((0, Box.empty.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(10))))))
    )

  val fullGameProposition = bobSk.vk.asProposition
    .and(Propositions.Contextual.HeightLock(50))
    .or(
      Propositions.Knowledge
        .HashLock(aliceCommit)
        .and(
          aliceSk.vk.asProposition
            .and(requiredInputBoxProposition)
            .or(bobSk.vk.asProposition)
        )
    )

  val halfGameProposition = Propositions.Contextual.RequiredBoxState(
    BoxLocations.Output,
    List((0, Box.empty.copy(evidence = fullGameProposition.dionAddress.typedEvidence)))
  )

  println(s"The address for the proposition is: ${fullGameProposition.dionAddress}")

  val unprovenTransaction: Transaction.Unproven = createUnprovenTransaction(
    List((halfGameProposition.dionAddress, Random.nextLong())),
    NonEmptyChain(Transaction.PolyOutput(aliceSk.vk.dionAddress, Sized.maxUnsafe(BigInt(10))))
  )

  val credential = Credential.Compositional.Or(
    fullGameProposition,
    List(
      Credential.Contextual.HeightLock(50),
      Credential.Knowledge.HashLock(Sized.strictUnsafe(Bytes(aliceSaltInput.value)), aliceValueInput),
      Credential.Knowledge.Curve25519(aliceSk, unprovenTransaction),
      Credential.Contextual.RequiredBoxState(
        BoxLocations.Input,
        List((0, Box.empty.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(10))))))
      )
    )
  )

  val proof = credential.proof
  println(proof)

  val transaction = transactionFromUnproven(unprovenTransaction, ListMap(fullGameProposition -> proof))
  println(transaction)

  implicit val context: VerificationContext[F] = new VerificationContext[F] {
    def currentTransaction: Transaction = transaction
    def currentHeight: Long = 1

    def inputBoxes: List[Box[Box.Value]] = List(
      Box(
        halfGameProposition.typedEvidence,
        unprovenTransaction.inputs.head._2,
        Box.Values.Poly(Sized.maxUnsafe(BigInt(10))),
        aliceValueInput
      )
    )
    def currentSlot: Slot = 1
  }

  println(s"Does the proof satisfy the proposition? ${fullGameProposition.isSatisfiedBy(proof).unsafeRunSync()}")
}

// (pk(Receiver) && sha256(H)) || (pk(Refundee) && older(10))
object HashTimeLockContract {
  import SetupSandbox._

  val aliceSaltInput = blake2b256.hash("test".getBytes)
  val aliceValueInput: Byte = 0
  val aliceCommit: Digest32 = Sized.strictUnsafe(Bytes(blake2b256.hash(aliceSaltInput.value :+ aliceValueInput).value))

  val proposition =
    aliceSk.vk.asProposition
      .and(Propositions.Knowledge.HashLock(aliceCommit))
      .or(bobSk.vk.asProposition.and(Propositions.Contextual.HeightLock(300)))

  println(s"The address for the proposition is: ${proposition}")
}

object RequiredBoxValue extends App {
  import SetupSandbox._

  val proposition = Propositions.Contextual.RequiredBoxState(
    BoxLocations.Output,
    List((0, Box.empty.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(10))))))
  )
  println(s"The address for the proposition is: ${proposition}")

  val unprovenTransaction: Transaction.Unproven = createUnprovenTransaction(
    List((proposition.dionAddress, Random.nextLong())),
    NonEmptyChain(Transaction.PolyOutput(address0, Sized.maxUnsafe(BigInt(10))))
  )

  val credential = Credential.Contextual.RequiredBoxState(
    BoxLocations.Output,
    List((0, Box.empty.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(10))))))
  )

  val proof = credential.proof
  println(proof)

  val transaction = transactionFromUnproven(unprovenTransaction, ListMap(proposition -> proof))
  println(transaction)

  implicit val context: VerificationContext[F] = new VerificationContext[F] {
    def currentTransaction: Transaction = transaction
    def currentHeight: Long = 1
    def inputBoxes: List[Box[Box.Value]] = List()
    def currentSlot: Slot = 1
  }

  println(s"Does the proof satisfy the proposition? ${proposition.isSatisfiedBy(proof).unsafeRunSync()}")
}

object NotTest extends App {
  import SetupSandbox._

  val proposition = Propositions.Compositional.Not(Propositions.Contextual.HeightLock(5))
  println(s"The address for the proposition is: ${proposition.dionAddress}")

  val unprovenTransaction: Transaction.Unproven = createUnprovenTransaction(
    List((proposition.dionAddress, Random.nextLong())),
    NonEmptyChain(Transaction.PolyOutput(address0, Sized.maxUnsafe(BigInt(10))))
  )

  val credential = Credential.Compositional.Not(
    proposition,
    List(
      Credential.Contextual.HeightLock(5)
    )
  )

  val proof = credential.proof
  println(proof)

  val transaction = transactionFromUnproven(unprovenTransaction, ListMap(proposition -> proof))
  println(transaction)

  implicit val context: VerificationContext[F] = new VerificationContext[F] {
    def currentTransaction: Transaction = transaction
    def currentHeight: Long = 3

    def inputBoxes: List[Box[Box.Value]] = List(
      Box(
        proposition.typedEvidence,
        unprovenTransaction.inputs.head._2,
        Box.Values.Poly(Sized.maxUnsafe(BigInt(10))),
        10
      )
    )

    def currentSlot: Slot = 1
  }

  println(s"Does the proof satisfy the proposition? ${proposition.isSatisfiedBy(proof).unsafeRunSync()}")

}
