package co.topl.credential.playground

import cats.data.NonEmptyChain
import cats.effect.unsafe.implicits.global
import co.topl.credential.Credential
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.{Base58, Sized}
import co.topl.scripting.GraalVMScripting
import co.topl.scripting.GraalVMScripting.GraalVMValuable
import co.topl.scripting.GraalVMScripting.instances._
import co.topl.typeclasses.implicits._
import co.topl.typeclasses.{KeyInitializer, VerificationContext}
import io.circe.Json
import io.circe.syntax._
import org.graalvm.polyglot.Value

import java.nio.charset.StandardCharsets
import scala.collection.immutable.ListMap
import scala.util.Random
import ModelGenerators._

object CredentialPlaygroundSean extends App {
  implicit val ed25519: Ed25519 = new Ed25519
  implicit val extendedEd25519: ExtendedEd25519 = ExtendedEd25519.precomputed()
  implicit val networkPrefix: NetworkPrefix = NetworkPrefix(1: Byte)

  // Exercise: Construct complex propositions and attempt to prove them using Credentials

  // Example:
  val party1SK: SecretKeys.Ed25519 = KeyInitializer[SecretKeys.Ed25519].random()
  val party2SK: SecretKeys.Curve25519 = KeyInitializer[SecretKeys.Curve25519].random()

  val party3Address: DionAddress = KeyInitializer[SecretKeys.Curve25519].random().vk.dionAddress

  val script1Proposition =
    """(ctx, args, utils) =>
      |  utils.base58Encode(
      |    utils.blake2b256Hash(args.secretMessage)
      |  ) == "9JNH95gJyEem86H1fmRDezhD2CHgsc9RpJikt5VRxn8W"""".stripMargin.jsProposition

  val script2Proposition =
    """(ctx, args, utils) => ctx.currentSlot > 400""".stripMargin.jsProposition

  val script3Proposition =
    """(ctx, args, utils) => ctx.currentTransaction.coinOutputs[0].value == "10"""".stripMargin.jsProposition

  val script4Proposition =
    """(ctx, args, utils) => {
      |  return true;
      |}""".stripMargin.jsProposition

  val proposition =
    party1SK.vk.asProposition
      .and(party2SK.vk.asProposition)
      .and(script1Proposition)
      .and(script2Proposition)
      .and(script3Proposition)
      .and(script4Proposition)
  println(proposition)

  val unprovenTransaction: Transaction.Unproven =
    ModelGenerators.arbitraryUnprovenTransaction.arbitrary.first.copy(
      inputs = NonEmptyChain(arbitraryTransactionUnprovenInput.arbitrary.first.copy(proposition = proposition)),
      outputs =
        NonEmptyChain(Transaction.Output(party3Address, Box.Values.Poly(Sized.maxUnsafe(BigInt(10))), minting = false))
    )

  val credential = Credential.Compositional.And(
    proposition,
    List(
      Credential.Knowledge.Ed25519(party1SK, unprovenTransaction),
      Credential.Knowledge.Curve25519(party2SK, unprovenTransaction),
      Credential.Script.JS(script1Proposition, Json.obj("secretMessage" -> "Foo".asJson)),
      Credential.Script.JS(script2Proposition, Json.Null),
      Credential.Script.JS(script3Proposition, Json.Null),
      Credential.Script.JS(script4Proposition, Json.Null)
    )
  )

  val proof = credential.proof
  println(proof)

  val transaction = unprovenTransaction.prove(_ => proof)
  println(transaction)

  type F[A] = cats.effect.IO[A]

  implicit val verificationContext: VerificationContext[F] =
    new VerificationContext[F] {
      def currentTransaction: Transaction = transaction

      def currentHeight: Long = 50L

      def currentSlot: Slot = 450L

      def inputBoxes: List[Box] = List()
    }

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

  val verificationResult: Boolean =
    proposition.isSatisfiedBy(proof).unsafeRunSync()
  println(verificationResult)

}

class VerificationUtils {

  def base58Encode(value: Value): Value =
    if (value.hasArrayElements) Value.asValue(Base58.encode(asScalaIterator(value).map(_.asByte()).toArray))
    else Value.asValue(Base58.encode(value.asString().getBytes(StandardCharsets.UTF_8)))

  def blake2b256Hash(value: Value): Value =
    if (value.hasArrayElements) Value.asValue(blake2b256.hash(asScalaIterator(value).map(_.asByte()).toArray))
    else Value.asValue(blake2b256.hash(value.asString().getBytes(StandardCharsets.UTF_8)).value)
}
