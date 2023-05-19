package co.topl.ledger.interpreters

import cats.Applicative
import cats.effect.Resource
import cats.implicits._
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra

// TODO: Implement in BN-1036
object TransactionRewardCalculator {

  def make[F[_]: Applicative]: Resource[F, TransactionRewardCalculatorAlgebra[F]] =
    Resource.pure((_: IoTransaction) => (Nil: Seq[Value]).pure[F])

}
