package co.topl.rpc.handlers

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{InvalidParametersError, RpcError, ThrowableData}
import co.topl.consensus.{Forger, ForgerInterface, KeyManagerInterface}
import co.topl.nodeView.{NodeViewHolderInterface, ReadableNodeView}
import co.topl.rpc.{ToplRpc, ToplRpcErrors}
import co.topl.utils.implicits._
import io.circe.Encoder

import scala.concurrent.Future

class AdminRpcHandlerImpls(
  forgerInterface:         ForgerInterface,
  keyManagerInterface:     KeyManagerInterface,
  nodeViewHolderInterface: NodeViewHolderInterface
)(implicit
  system:           ActorSystem[_],
  throwableEncoder: Encoder[ThrowableData],
  timeout:          Timeout
) extends ToplRpcHandlers.Admin {

  import system.executionContext

  override val unlockKeyfile: ToplRpc.Admin.UnlockKeyfile.rpc.ServerHandler =
    params =>
      keyManagerInterface
        .unlockKey(params.address, params.password)
        .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
        .subflatMap {
          case addr if params.address == addr.show => Map(addr -> "unlocked").asRight
          case _ => InvalidParametersError.adhoc("address", "Decrypted address does not match requested address").asLeft
        }

  override val lockKeyfile: ToplRpc.Admin.LockKeyfile.rpc.ServerHandler =
    params =>
      keyManagerInterface
        .lockKey(params.address)
        .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
        .map(_ => Map(params.address -> "locked"))

  override val generateKeyfile: ToplRpc.Admin.GenerateKeyfile.rpc.ServerHandler =
    params =>
      keyManagerInterface
        .createKey(params.password)
        .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
        .map(ToplRpc.Admin.GenerateKeyfile.Response)

  override val importSeedPhrase: ToplRpc.Admin.ImportSeedPhrase.rpc.ServerHandler =
    params =>
      keyManagerInterface
        .importKey(params.password, params.seedPhrase, params.seedPhraseLang)
        .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
        .map(ToplRpc.Admin.ImportSeedPhrase.Response)

  override val listOpenKeyfiles: ToplRpc.Admin.ListOpenKeyfiles.rpc.ServerHandler =
    _ =>
      keyManagerInterface
        .listOpenKeyfiles()
        .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
        .map(ToplRpc.Admin.ListOpenKeyfiles.Response)

  override val startForging: ToplRpc.Admin.StartForging.rpc.ServerHandler =
    _ =>
      forgerInterface
        .startForging()
        .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
        .map(_ => ToplRpc.Admin.StartForging.Response("START forging signal sent"))

  override val stopForging: ToplRpc.Admin.StopForging.rpc.ServerHandler =
    _ =>
      forgerInterface
        .stopForging()
        .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
        .map(_ => ToplRpc.Admin.StopForging.Response("STOP forging signal sent"))

  override val updateRewardsAddress: ToplRpc.Admin.UpdateRewardsAddress.rpc.ServerHandler =
    params =>
      keyManagerInterface
        .updateRewardsAddress(params.address)
        .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
        .map(_ => ToplRpc.Admin.UpdateRewardsAddress.Response(s"Updated reward address to ${params.address.show}"))

  override val getRewardsAddress: ToplRpc.Admin.GetRewardsAddress.rpc.ServerHandler =
    _ =>
      keyManagerInterface
        .getRewardsAddress
        .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
        .map(ToplRpc.Admin.GetRewardsAddress.Response)

  override val status: ToplRpc.Admin.Status.rpc.ServerHandler =
    _ =>
      for {
        forgerStatus <- forgerInterface
          .checkForgerStatus()
          .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
          .map {
            case Forger.Active        => "active"
            case Forger.Idle          => "idle"
            case Forger.Uninitialized => "uninitialized"
          }
        mempoolSize <- withNodeView(_.memPool.size)
      } yield ToplRpc.Admin.Status.Response(forgerStatus, mempoolSize)

  private def withNodeView[T](f: ReadableNodeView => T): EitherT[Future, RpcError, T] =
    readFromNodeViewHolder(nodeViewHolderInterface)(f)

}
