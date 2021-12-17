package co.topl.credential

import cats.data.NonEmptyChain
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.typeclasses.{KeyInitializer, VerificationContext}
import co.topl.typeclasses.implicits._

import scala.annotation.tailrec
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

  type F[A] = cats.Id[A]

  implicit val verificationContext: VerificationContext[F] =
    new VerificationContext[F] {
      def currentTransaction: Transaction = transaction

      def currentHeight: Long = 50L
    }

  val verificationResult: Boolean =
    proposition.isSatisifiedBy[F](proof)
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
  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = ExtendedEd25519.precomputed()
  implicit val networkPrefix: NetworkPrefix = NetworkPrefix(1: Byte)

  val party1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val party2SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()

  val parties: Seq[SecretKeys.Curve25519] = for (i <- 1 to Random.between(2, 50)) yield i match {
    case _ => KeyInitializer[SecretKeys.Curve25519].random()
  }

  println(parties.size)

  val party3Address: DionAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.dionAddress

  @tailrec
  def recurseOr(propositions: Seq[Proposition], acc: Proposition): Propositions.Compositional.Or = propositions match {
    case prop +: Nil  => acc.or(prop)
    case prop +: tail => recurseOr(tail, acc.or(prop))
  }

  val partiesProps = parties.map(_.vk.asProposition)

  val proposition: Propositions.Compositional.Or = recurseOr(partiesProps.tail, partiesProps.head)

  println(s"proposition: $proposition")

  val unprovenTransaction: Transaction.Unproven = Transaction.Unproven(
    inputs = List((proposition.dionAddress, Random.nextLong())),
    feeOutput = None,
    coinOutputs = NonEmptyChain(Transaction.PolyOutput(party3Address, Sized.maxUnsafe(BigInt(10)))),
    fee = Sized.maxUnsafe(BigInt(5)),
    timestamp = System.currentTimeMillis(),
    data = None,
    minting = false
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

  type F[A] = cats.Id[A]

  implicit val verificationContext: VerificationContext[F] =
    new VerificationContext[F] {
      def currentTransaction: Transaction = transaction

      def currentHeight: Long = 50L
    }

  val verificationResult: Boolean =
    proposition.isSatisifiedBy[F](proof)
  println(verificationResult)

  proofs.foreach(p => println(proposition.isSatisifiedBy[F](p)))

  val propMatrix = for {
    
  } yield ???

  println(Tabulator.format(Seq(Seq.range(1, parties.size + 1).map(_.toString) ++ "Result")))

}
