package co.topl.genus

import co.topl.genus.types.{Block, Transaction}

package object algebras {
  type TransactionsQueryService[F[_]] = QueryService[F, Transaction]
  type TransactionsSubscriptionService[F[_]] = SubscriptionService[F, Transaction]

  type BlocksQueryService[F[_]] = QueryService[F, Block]
  type BlocksSubscriptionService[F[_]] = SubscriptionService[F, Block]
}
