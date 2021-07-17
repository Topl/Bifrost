package co.topl.rpc.handlers

import akka.actor.ActorSystem
import akka.dispatch.Dispatchers
import akka.util.Timeout
import cats.implicits._
import co.topl.akkahttprpc.{InvalidParametersError, RpcError}
import co.topl.consensus.{ForgerInterface, KeyManagerInterface}
import co.topl.rpc.{ToplRpc, ToplRpcErrors}

import scala.concurrent.ExecutionContext

class AdminRpcHandlerImpls(forgerInterface: ForgerInterface, keyManagerInterface: KeyManagerInterface)(implicit
  system:                                   ActorSystem,
  timeout:                                  Timeout
) extends ToplRpcHandlers.Admin {

  implicit private val ec: ExecutionContext =
    system.dispatchers.lookup(Dispatchers.DefaultBlockingDispatcherId)

  override val unlockKeyfile: ToplRpc.Admin.UnlockKeyfile.rpc.ServerHandler =
    params =>
      keyManagerInterface
        .unlockKey(params.address, params.password)
        .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
        .subflatMap {
          case addr if params.address == addr.toString => Map(addr -> "unlocked").asRight
          case _                                       => InvalidParametersError.adhoc("address", "Decrypted address does not match requested address").asLeft
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
        .map(_ => ToplRpc.Admin.UpdateRewardsAddress.Response(params.address.toString))

  override val getRewardsAddress: ToplRpc.Admin.GetRewardsAddress.rpc.ServerHandler =
    _ =>
      keyManagerInterface
        .getRewardsAddress()
        .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
        .map(ToplRpc.Admin.GetRewardsAddress.Response)

}
