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
import co.topl.modifier.box.{AssetCode, AssetValue}
import co.topl.modifier.transaction.AssetTransfer
import co.topl.rpc.ToplRpc
import co.topl.rpc.ToplRpc.Transaction.RawAssetTransfer
import co.topl.rpc.implicits.client._
import co.topl.utils.NetworkType.NetworkPrefix
import com.nike.fleam.implicits._

import scala.concurrent.ExecutionContext

object AssetTransferFlow {

  case class Req(assetCode: AssetCode, amount: Int, minting: Boolean, from: Address, to: Address)

  private def params(request: Req) =
    ToplRpc.Transaction.RawAssetTransfer.Params(
      propositionType = PublicKeyPropositionCurve25519.typeString,
      sender = NonEmptyChain(request.from),
      recipients = NonEmptyChain((request.to, AssetValue(request.amount, request.assetCode))),
      fee = 100,
      changeAddress = request.from,
      consolidationAddress = request.from,
      minting = request.minting,
      data = None
    )

  def apply(keys:    ActorRef[KeysActor.Command])(implicit
    networkPrefix:   NetworkPrefix,
    requestModifier: RequestModifier,
    ec:              ExecutionContext,
    actorSystem:     ActorSystem,
    timeout:         Timeout,
    scheduler:       Scheduler
  ): Flow[Req, Either[RpcClientFailure, AssetTransfer[PublicKeyPropositionCurve25519]], NotUsed] =
    Flow[Req]
      .map(params)
      .mapAsync(1)(RawAssetTransfer.rpc(_).value)
      .viaRight(
        Flow[RawAssetTransfer.Response]
          .map(_.rawTx)
          .mapAsync(1) { tx =>
            for {
              signedTx <- signMessage(tx.from.head._1, tx.messageToSign, keys)
            } yield tx.copy(attestation = signedTx)
          }
      )

}
