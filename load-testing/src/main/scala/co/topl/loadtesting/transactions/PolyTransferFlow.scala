package co.topl.loadtesting.transactions

import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.{ActorRef, Scheduler}
import akka.stream.scaladsl.Flow
import akka.util.Timeout
import cats.data.NonEmptyChain
import co.topl.akkahttprpc.implicits.client._
import co.topl.akkahttprpc.{RequestModifier, RpcClientFailure}
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.loadtesting.KeysActor
import co.topl.modifier.transaction.PolyTransfer
import co.topl.rpc.ToplRpc
import co.topl.rpc.ToplRpc.Transaction.RawPolyTransfer
import co.topl.rpc.implicits.client._
import co.topl.utils.NetworkType.NetworkPrefix
import com.nike.fleam.implicits._

import scala.collection.immutable.ListMap
import scala.concurrent.{ExecutionContext, Future}

object PolyTransferFlow {

  case class Req(from: Address, to: Address, amount: Int)

  private def params(request: Req) =
    ToplRpc.Transaction.RawPolyTransfer.Params(
      propositionType = PublicKeyPropositionCurve25519.typeString,
      sender = NonEmptyChain(request.from),
      recipients = NonEmptyChain((request.to, request.amount)),
      fee = 100,
      changeAddress = request.from,
      data = None
    )

  private def signMessage(address: Address,
                          message: Array[Byte],
                          keys: ActorRef[KeysActor.Command]
                         )(implicit timeout: Timeout, scheduler: Scheduler
  ): Future[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]] =
    keys.ask(KeysActor.SignMessage(address, message, _))

  def apply(keys: ActorRef[KeysActor.Command])(implicit
                                              networkPrefix: NetworkPrefix,
                                              requestModifier: RequestModifier,
                                              ec: ExecutionContext,
                                              actorSystem: ActorSystem,
                                              timeout: Timeout,
                                              scheduler: Scheduler
  ): Flow[Req, Either[RpcClientFailure, PolyTransfer[PublicKeyPropositionCurve25519]], NotUsed] =
    Flow[Req]
      .map(params)
      .mapAsync(1)(RawPolyTransfer.rpc(_).value)
      .viaRight(
        Flow[RawPolyTransfer.Response]
          .map(_.rawTx)
          .mapAsync(1)(tx => {
            for {
              signedTx <- signMessage(tx.from.head._1, tx.messageToSign, keys)
            } yield tx.copy(attestation = signedTx)
          })
      )

}
