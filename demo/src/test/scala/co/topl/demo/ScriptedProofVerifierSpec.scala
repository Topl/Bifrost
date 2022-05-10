package co.topl.demo

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
import co.topl.models._
import co.topl.scripting.GraalVMScripting
import co.topl.scripting.GraalVMScripting.GraalVMValuable
import co.topl.scripting.GraalVMScripting.instances._
import co.topl.typeclasses.VerificationContext
import co.topl.typeclasses.implicits._
import io.circe.Json
import io.circe.syntax._
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import co.topl.models.ModelGenerators._

class ScriptedProofVerifierSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {

  import ScriptedProofVerifierSpec._

  behavior of "JS Propositions"

  it should "verify to true for a script using context and args" in {
    forAll { unproven: Transaction.Unproven =>
      val proposition = Propositions.Script.JS(
        Propositions.Script.JS.JSScript(
          """(ctx, args) => ctx.currentHeight > 30 && args["foo"] == 37""".stripMargin
        )
      )

      val proof = Proofs.Script.JS(
        Json.obj("foo" -> 37.asJson).toString
      )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(40L)

      val transaction = unproven.prove(_ => proof)

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      (() => context.currentSlot)
        .expects()
        .once()
        .returning(400L)

      val result =
        proof.satisfies[F](proposition).unsafeRunSync()

      val expected = true

      result shouldBe expected

    }
  }

  it should "verify to false" in {
    forAll { unproven: Transaction.Unproven =>
      val proposition = Propositions.Script.JS(
        Propositions.Script.JS.JSScript(
          """(ctx, args) => ctx.currentHeight > 30 && args["foo"] == 37""".stripMargin
        )
      )

      val proof = Proofs.Script.JS(
        Json.obj("foo" -> 37.asJson).toString
      )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(20L)

      val transaction = unproven.prove(_ => proof)

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      (() => context.currentSlot)
        .expects()
        .once()
        .returning(200L)

      val result =
        proof.satisfies[F](proposition).unsafeRunSync()

      val expected = false

      result shouldBe expected

    }
  }

  it should "verify to true for a script using the current transaction" in {
    forAll { unproven: Transaction.Unproven =>
      val outAddr = unproven.outputs.head.dionAddress.allBytes.toBase58
      val proposition = Propositions.Script.JS(
        Propositions.Script.JS.JSScript(
          raw"""(ctx, args) =>
               |    ctx.currentTransaction.coinOutputs[args.outputIndex].address == "$outAddr";
               |""".stripMargin
        )
      )

      val proof = Proofs.Script.JS(Json.obj("outputIndex" -> 0.asJson).toString)

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(40L)

      val transaction = unproven.prove(_ => proof)

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      (() => context.currentSlot)
        .expects()
        .once()
        .returning(400L)

      val result =
        proof.satisfies[F](proposition).unsafeRunSync()

      val expected = true

      result shouldBe expected
    }
  }
}

object ScriptedProofVerifierSpec {

  type F[A] = IO[A]

  implicit val networkPrefix: NetworkPrefix = NetworkPrefix(1)

  implicit val ed25519: Ed25519 = new Ed25519()
  implicit val extendedEd25519: ExtendedEd25519 = new ExtendedEd25519

  implicit val jsExecutor: Propositions.Script.JS.JSScript => F[(Json, Json) => F[Boolean]] =
    s =>
      GraalVMScripting
        .jsExecutor[F, Boolean](s.value)
        .map(f =>
          Function.untupled(
            f.compose[(Json, Json)] { t =>
              Seq(GraalVMValuable[Json].toGraalValue(t._1), GraalVMValuable[Json].toGraalValue(t._2))
            }
          )
        )
}
