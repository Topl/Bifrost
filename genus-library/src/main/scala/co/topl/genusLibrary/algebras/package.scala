package co.topl.genusLibrary

import co.topl.genusLibrary.failure.Failure

package object algebras {
  /*
   * The idea of having both ServiceResponse and StoreResponse is to have a semantic distinction.
   * A Service response means that it's a response that comes from the network and it's a service layer.
   * It could be either a Node or a third party API.
   *
   * On the other hand, a Store response also means that it's a response that comes from the network,
   * but it's a Store/Repository layer.
   * I think it's a great way to show the developer who may use these methods that the response
   * comes from a DB of some sorts.
   */
  type ServiceResponse[F[_], T] = F[Either[Failure, T]]
  type StoreResponse[F[_], T] = ServiceResponse[F, T]

  type SequenceResponse[F[_], G[_], T] = F[G[Either[Failure, T]]]
}
