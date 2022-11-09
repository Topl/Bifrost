package co.topl.genusLibrary

import co.topl.genusLibrary.failure.Failure

package object algebras {
  type ServiceResponse[F[_], T] = F[Either[Failure, T]]
  type StoreResponse[F[_], T] = ServiceResponse[F, T]
}
