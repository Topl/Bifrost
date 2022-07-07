package co.topl.credential.playground

import cats.data.NonEmptyChain
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.credential.Credential
import co.topl.crypto.generation.KeyInitializer
import co.topl.crypto.generation.KeyInitializer.Instances.{curve25519Initializer, ed25519Initializer}
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._

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

  implicit val curve25519: Curve25519 = new Curve25519
  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = new ExtendedEd25519

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

  val curve25519Sk: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()
  val ed25519Sk: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val aliceSk: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()
  val bobSk: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()

  val address0: SpendingAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.spendingAddress
  val address1: SpendingAddress = curve25519Sk.vk.asProposition.and(ed25519Sk.vk.asProposition).spendingAddress

  def createUnprovenTransaction(
    inputs:  NonEmptyChain[Transaction.Unproven.Input],
    outputs: NonEmptyChain[Transaction.Output]
  ): Transaction.Unproven =
    Transaction.Unproven(
      inputs.toChain,
      outputs.toChain,
      chronology = Transaction.Chronology(System.currentTimeMillis(), 0, Long.MaxValue),
      data = None
    )
}

object CredentialPlaygroundJAA extends App {
  import SetupSandbox._

  val proposition = curve25519Sk.vk.asProposition.and(ed25519Sk.vk.asProposition)
  println(s"The address for the proposition is: ${proposition.spendingAddress}")

  val unprovenTransaction: Transaction.Unproven = createUnprovenTransaction(
    NonEmptyChain(ModelGenerators.arbitraryTransactionUnprovenInput.arbitrary.first.copy(proposition = proposition)),
    NonEmptyChain(
      Transaction.Output(fullAddress(address0), Box.Values.Poly(Sized.maxUnsafe(BigInt(10))), minting = false)
    )
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

  val transaction = unprovenTransaction.prove(_ => proof)
  println(transaction)
}

object RequiredOutput extends App {
  import SetupSandbox._

  val requiredBoxProposition = Propositions.Contextual.RequiredBoxState(
    List(
      Box.empty.copy(evidence = address0.typedEvidence) -> BoxLocations.Output(0)
    ) // todo: helper function name for Box.empty.copy
  )
  val proposition = curve25519Sk.vk.asProposition.and(requiredBoxProposition)
  println(s"The address for the proposition is: ${proposition.spendingAddress}")

  val unprovenTransaction: Transaction.Unproven = createUnprovenTransaction(
    NonEmptyChain(arbitraryTransactionUnprovenInput.arbitrary.first.copy(proposition = proposition)),
    NonEmptyChain(
      Transaction.Output(fullAddress(address0), Box.Values.Poly(Sized.maxUnsafe(BigInt(10))), minting = false)
    )
  )

  val credential = Credential.Compositional.And(
    proposition,
    List(
      Credential.Knowledge.Curve25519(curve25519Sk, unprovenTransaction),
      Credential.Contextual
        .RequiredBoxState(
          List(
            Box.empty.copy(evidence = address0.typedEvidence) -> BoxLocations.Output(0)
          )
        )
    )
  )

  val proof = credential.proof
  println(proof)

  val transaction = unprovenTransaction.prove(_ => proof)
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

  val aliceCommit: Digest32 =
    new Blake2b256().hash(Bytes.encodeUtf8("test" + "1").getOrElse(???))

  val requiredInputBoxProposition =
    Propositions.Contextual.RequiredBoxState(
      List(
        Box.empty.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(10)))) -> BoxLocations.Input(0)
      )
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
    List(
      Box.empty.copy(evidence = fullGameProposition.spendingAddress.typedEvidence) -> BoxLocations.Output(0)
    )
  )

  println(s"The address for the proposition is: ${fullGameProposition.spendingAddress}")

  val unprovenTransaction: Transaction.Unproven = createUnprovenTransaction(
    NonEmptyChain(arbitraryTransactionUnprovenInput.arbitrary.first.copy(proposition = halfGameProposition)),
    NonEmptyChain(
      Transaction.Output(
        fullAddress(aliceSk.vk.spendingAddress),
        Box.Values.Poly(Sized.maxUnsafe(BigInt(10))),
        minting = false
      )
    )
  )

  val credential = Credential.Compositional.Or(
    fullGameProposition,
    List(
      Credential.Contextual.HeightLock(50),
      Credential.Knowledge.HashLock(Bytes.encodeUtf8("test" + "1").getOrElse(???)),
      Credential.Knowledge.Curve25519(aliceSk, unprovenTransaction),
      Credential.Contextual.RequiredBoxState(
        List(
          Box.empty.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(10)))) -> BoxLocations.Input(0)
        )
      )
    )
  )

  val proof = credential.proof
  println(proof)

  val transaction = unprovenTransaction.prove(_ => proof)
  println(transaction)

}

// (pk(Receiver) && sha256(H)) || (pk(Refundee) && older(10))
object HashTimeLockContract extends App {
  import SetupSandbox._

  val credential = Credential.Knowledge.HashLock(Bytes.encodeUtf8("test").getOrElse(???))

  val proposition = credential.proposition
  println(s"The proposition is: ${proposition}")
  println(s"The address for the proposition is: ${fullAddress(proposition.spendingAddress)}")

  val unprovenTransaction: Transaction.Unproven =
    createUnprovenTransaction(
      NonEmptyChain(
        Transaction.Unproven
          .Input(arbitraryBoxId.arbitrary.first, proposition, arbitraryBoxValue.arbitrary.first)
      ),
      NonEmptyChain(arbitraryTransactionOutput.arbitrary.first)
    )

  val proof = credential.proof
  println(proof)

  val transaction = unprovenTransaction.prove(_ => proof)
  println(transaction)
}

object RequiredBoxValue extends App {
  import SetupSandbox._

  val proposition = Propositions.Contextual.RequiredBoxState(
    List(
      Box.empty.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(10)))) -> BoxLocations.Output(0)
    )
  )
  println(s"The address for the proposition is: ${proposition}")

  val unprovenTransaction: Transaction.Unproven = createUnprovenTransaction(
    NonEmptyChain(arbitraryTransactionUnprovenInput.arbitrary.first.copy(proposition = proposition)),
    NonEmptyChain(
      Transaction.Output(fullAddress(address0), Box.Values.Poly(Sized.maxUnsafe(BigInt(10))), minting = false)
    )
  )

  val credential = Credential.Contextual.RequiredBoxState(
    List(
      Box.empty.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(10)))) -> BoxLocations.Output(0)
    )
  )

  val proof = credential.proof
  println(proof)

  val transaction = unprovenTransaction.prove(_ => proof)
  println(transaction)
}

object NotTest extends App {
  import SetupSandbox._

  val proposition = Propositions.Compositional.Not(Propositions.Contextual.HeightLock(5))
  println(s"The address for the proposition is: ${proposition.spendingAddress}")

  val unprovenTransaction: Transaction.Unproven = createUnprovenTransaction(
    NonEmptyChain(arbitraryTransactionUnprovenInput.arbitrary.first.copy(proposition = proposition)),
    NonEmptyChain(
      Transaction.Output(fullAddress(address0), Box.Values.Poly(Sized.maxUnsafe(BigInt(10))), minting = false)
    )
  )

  val credential = Credential.Compositional.Not(
    proposition,
    List(
      Credential.Contextual.HeightLock(5)
    )
  )

  val proof = credential.proof
  println(proof)

  val transaction = unprovenTransaction.prove(_ => proof)
  println(transaction)

}
