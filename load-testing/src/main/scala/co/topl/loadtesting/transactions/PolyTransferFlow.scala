package co.topl.loadtesting.transactions

import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.typed.{ActorRef, Scheduler}
import akka.stream.scaladsl.Flow
import akka.util.Timeout
import cats.data.NonEmptyChain
import co.topl.akkahttprpc.implicits.client._
import co.topl.akkahttprpc.{RequestModifier, RpcClientFailure}
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.loadtesting.KeysActor
import co.topl.modifier.transaction.PolyTransfer
import co.topl.rpc.ToplRpc
import co.topl.rpc.ToplRpc.Transaction.RawPolyTransfer
import co.topl.rpc.implicits.client._
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import com.nike.fleam.implicits._

import scala.concurrent.ExecutionContext
import scala.util.Random

object PolyTransferFlow {

  private val random = new Random()

  case class Req(from: Address, to: Address, amount: Int)

  private def params(request: Req) =
    ToplRpc.Transaction.RawPolyTransfer.Params(
      propositionType = PublicKeyPropositionCurve25519.typeString,
      sender = NonEmptyChain(request.from),
      recipients = NonEmptyChain((request.to, request.amount)),
      fee = 100,
      changeAddress = request.from,
      data = Some(Latin1Data.unsafe(random.alphanumeric.take(random.between(0, 127)).mkString))
    )

  def apply(keys:    ActorRef[KeysActor.Command])(implicit
    networkPrefix:   NetworkPrefix,
    requestModifier: RequestModifier,
    ec:              ExecutionContext,
    actorSystem:     ActorSystem,
    timeout:         Timeout,
    scheduler:       Scheduler
  ): Flow[Req, Either[RpcClientFailure, PolyTransfer[PublicKeyPropositionCurve25519]], NotUsed] =
    Flow[Req]
      .map(params)
      .mapAsync(1)(RawPolyTransfer.rpc(_).value)
      .viaRight(
        Flow[RawPolyTransfer.Response]
          .map(_.rawTx)
          .mapAsync(1) { tx =>
            for {
              signedTx <- signMessage(tx.from.head._1, tx.messageToSign, keys)
            } yield tx.copy(attestation = signedTx)
          }
      )

}
