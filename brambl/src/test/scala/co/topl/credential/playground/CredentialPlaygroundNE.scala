package co.topl.credential.playground

import cats.data.Chain
import cats.effect.unsafe.implicits.global
import co.topl.credential.Credential
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
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

import scala.annotation.tailrec
import scala.util.Random
import ModelGenerators._
import co.topl.crypto.generation.KeyInitializer

object CredentialPlaygroundNE extends App {
  type F[A] = cats.effect.IO[A]

  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = ExtendedEd25519.instance

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

  // Example:
  val party1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val party2SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()

  val party3Address: SpendingAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.spendingAddress

  val proposition: Propositions.Compositional.And = party1SK.vk.asProposition.and(party2SK.vk.asProposition)
  println(proposition)

  val unprovenTransaction: Transaction.Unproven =
    ModelGenerators.arbitraryUnprovenTransaction.arbitrary.first.copy(
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

  val proof: Proof = credential.proof
  println(proof)

  val transaction = unprovenTransaction.prove(_ => proof)
  println(transaction)

  implicit val verificationContext: VerificationContext[F] =
    new VerificationContext[F] {
      def currentTransaction: Transaction = transaction

      def currentHeight: Long = 50L

      def inputBoxes: List[Box] = List(
        Box(
          proposition.typedEvidence,
          Box.Values.Poly(Sized.maxUnsafe(BigInt(10)))
        )
      )

      def currentSlot: Slot = 1
    }

  val verificationResult =
    proposition.isSatisfiedBy(proof).unsafeRunSync()
  println(verificationResult)

}

object Tabulator {

  def format(table: Seq[Seq[Any]]) = table match {
    case Seq() => ""
    case _ =>
      val sizes = for (row <- table) yield (for (cell <- row) yield if (cell == null) 0 else cell.toString.length)
      val colSizes = for (col <- sizes.transpose) yield col.max
      val rows = for (row <- table) yield formatRow(row, colSizes)
      formatRows(rowSeparator(colSizes), rows)
  }

  def formatRows(rowSeparator: String, rows: Seq[String]): String = (rowSeparator ::
    rows.head ::
    rowSeparator ::
    rows.tail.toList :::
    rowSeparator ::
    List()).mkString("\n")

  def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
    val cells = (for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item))
    cells.mkString("|", "|", "|")
  }

  def rowSeparator(colSizes: Seq[Int]) = colSizes map { "-" * _ } mkString ("+", "+", "+")
}

object TruthTable extends App {
  type F[A] = cats.effect.IO[A]

  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = ExtendedEd25519.instance

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

  val party1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val party2SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()

  val parties: Seq[SecretKeys.Curve25519] = for (i <- 1 to (Random.nextInt(49) + 1)) yield i match {
    case _ => KeyInitializer[SecretKeys.Curve25519].random()
  }

  println(parties.size)

  val party3Address: SpendingAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.spendingAddress

  @tailrec
  def recurseOr(propositions: Seq[Proposition], acc: Proposition): Propositions.Compositional.Or = propositions match {
    case prop +: Nil  => acc.or(prop)
    case prop +: tail => recurseOr(tail, acc.or(prop))
  }

  val partiesProps = parties.map(_.vk.asProposition)

  val proposition: Propositions.Compositional.Or = recurseOr(partiesProps.tail, partiesProps.head)

  println(s"proposition: $proposition")

  val unprovenTransaction: Transaction.Unproven =
    ModelGenerators.arbitraryUnprovenTransaction.arbitrary.first.copy(
      inputs = Chain(arbitraryTransactionUnprovenInput.arbitrary.first.copy(proposition = proposition)),
      outputs = Chain(
        Transaction.Output(fullAddress(party3Address), Box.Values.Poly(Sized.maxUnsafe(BigInt(10))), minting = false)
      )
    )

  val credential = Credential.Compositional.Or(
    proposition,
    parties.map(p => Credential.Knowledge.Curve25519(p, unprovenTransaction))
  )

  val proof = credential.proof

  val proofs: Seq[Proof] = parties.map { p =>
    Credential.Compositional.Or(proposition, List(Credential.Knowledge.Curve25519(p, unprovenTransaction))).proof
  }
  println(proof)

  val transaction = unprovenTransaction.prove(_ => proof)
  println(transaction)

  implicit val verificationContext: VerificationContext[F] =
    new VerificationContext[F] {
      def currentTransaction: Transaction = transaction

      def currentHeight: Long = 50L

      def inputBoxes: List[Box] = List(
        Box(
          proposition.typedEvidence,
          Box.Values.Poly(Sized.maxUnsafe(BigInt(10)))
        )
      )

      def currentSlot: Slot = 1
    }

  val verificationResult: Boolean =
    proposition.isSatisfiedBy(proof).unsafeRunSync()
  println(verificationResult)

  proofs.foreach(p => println(proposition.isSatisfiedBy(p).unsafeRunSync()))
//
//  val propMatrix = for {
//
//  } yield ???

  println(Tabulator.format(Seq(Seq.range(1, parties.size + 1).map(_.toString) ++ "Result")))

}
