package co.topl.ledger.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.ledger.algebras.TransactionSyntaxValidationAlgebra
import co.topl.ledger.models._
import co.topl.models.ModelGenerators._
import co.topl.models.{ModelGenerators, Transaction, TypedIdentifier}
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class BodySyntaxValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("validation should fail if any transaction is syntactically invalid") {
    PropF.forAllF { transaction: Transaction =>
      withMock {
        val body = List(transaction.id.asTypedBytes)
        for {
          fetchTransaction <- mockFunction[TypedIdentifier, F[Transaction]].pure[F]
          _ = fetchTransaction.expects(transaction.id.asTypedBytes).once().returning(transaction.pure[F])
          transactionSyntaxValidation = mock[TransactionSyntaxValidationAlgebra[F]]
          _ = (transactionSyntaxValidation.validate _)
            .expects(transaction)
            .once()
            .returning((TransactionSyntaxErrors.EmptyInputs: TransactionSyntaxError).invalidNec[Transaction].pure[F])
          underTest <- BodySyntaxValidation.make[F](fetchTransaction, transactionSyntaxValidation)
          result    <- underTest.validate(body)
          _         <- IO(result.isInvalid).assert
        } yield ()
      }
    }
  }
}
