package co.topl.ledger.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.codecs.bytes.tetra.instances._
import co.topl.node.models.BlockBody
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class BodySyntaxValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("validation should fail if any transaction is syntactically invalid") {
    PropF.forAllF { transaction: IoTransaction =>
      withMock {
        val body = BlockBody(List(transaction.id))
        for {
          fetchTransaction <- mockFunction[Identifier.IoTransaction32, F[IoTransaction]].pure[F]
          _ = fetchTransaction.expects(transaction.id).once().returning(transaction.pure[F])
          transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
          _ = (transactionSyntaxValidation.validate _)
            .expects(transaction)
            .once()
            .returning((TransactionSyntaxError.EmptyInputs: TransactionSyntaxError).invalidNec[IoTransaction].pure[F])
          underTest <- BodySyntaxValidation.make[F](fetchTransaction, transactionSyntaxValidation)
          result    <- underTest.validate(body)
          _         <- IO(result.isInvalid).assert
        } yield ()
      }
    }
  }
}
