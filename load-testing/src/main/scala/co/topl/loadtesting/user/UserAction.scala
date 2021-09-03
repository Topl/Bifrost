package co.topl.loadtesting.user

import akka.NotUsed
import akka.stream.scaladsl.Flow
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.rpc.ToplRpc.NodeView.Balances

import scala.collection.immutable.ListMap
import scala.concurrent.Future

abstract class UserAction[A, F, S] {

  def actionFlow(
    action:   A,
    address:  Address,
    contacts: List[Address],
    sign:     Array[Byte] => Future[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]]
  ): Flow[Balances.Entry, Either[F, S], NotUsed]

}
