package co.topl.http.rpc

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import cats.data.{EitherT, Kleisli}
import cats.implicits._
import co.topl.akkahttprpc.{CustomError, Rpc, RpcError, ThrowableData}
import co.topl.attestation.Address
import co.topl.modifier.ModifierId
import co.topl.modifier.box.AssetCode
import co.topl.nodeView.CurrentView
import co.topl.nodeView.history.{History, HistoryDebug}
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.AppContext
import co.topl.utils.NetworkType
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.Encoder
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

import java.security.SecureRandom
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class BifrostRpcHandlers(
  debug: BifrostRpcHandlers.Debug,
  utils: BifrostRpcHandlers.Utils
)

object BifrostRpcHandlers {

  trait Debug {
    def delay: ToplRpc.Debug.Delay.rpc.ServerHandler
    def myBlocks: ToplRpc.Debug.MyBlocks.rpc.ServerHandler
    def generators: ToplRpc.Debug.Generators.rpc.ServerHandler
    def idsFromHeight: ToplRpc.Debug.IdsFromHeight.rpc.ServerHandler
  }

  trait Utils {
    def seed: ToplRpc.Utils.Seed.rpc.ServerHandler
    def seedOfLength: ToplRpc.Utils.SeedOfLength.rpc.ServerHandler
    def hashBlake2b256: ToplRpc.Utils.HashBlake2b256.rpc.ServerHandler
    def generateAssetCode: ToplRpc.Utils.GenerateAssetCode.rpc.ServerHandler
    def checkValidAddress: ToplRpc.Utils.CheckValidAddress.rpc.ServerHandler
  }
}

object BifrostRpcHandlerImpls {

  class Debug(
    appContext:        AppContext,
    nodeViewHolderRef: ActorRef,
    forgerRef:         ActorRef
  )(implicit
    ec:               ExecutionContext,
    timeout:          Timeout,
    throwableEncoder: Encoder[ThrowableData]
  ) extends BifrostRpcHandlers.Debug {

    import co.topl.consensus.Forger
    import co.topl.nodeView.NodeViewHolder

    implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

    type CV = CurrentView[History, State, MemPool]

    private def currentView(): EitherT[Future, RpcError[_], CV] =
      EitherT.liftF((nodeViewHolderRef ? NodeViewHolder.ReceivableMessages.GetDataFromCurrentView).mapTo[CV])

    private def listKeys(): EitherT[Future, RpcError[_], Set[Address]] =
      EitherT.liftF((forgerRef ? Forger.ReceivableMessages.ListKeys).mapTo[Set[Address]])

    override val delay: ToplRpc.Debug.Delay.rpc.ServerHandler =
      Kleisli[Rpc.ServerResponse, ToplRpc.Debug.Delay.Params, ToplRpc.Debug.Delay.Response](params =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
          delay <- EitherT.fromEither[Future](
            historyDebug
              .averageDelay(params.blockId, params.numBlocks)
              .toEither
              .leftMap(CustomError.fromThrowable(-32099, "Unexpected server error", _): RpcError[_])
          )
        } yield ToplRpc.Debug.Delay.Response(s"$delay milliseconds")
      )

    override val myBlocks: ToplRpc.Debug.MyBlocks.rpc.ServerHandler =
      Kleisli[Rpc.ServerResponse, ToplRpc.Debug.MyBlocks.Params, ToplRpc.Debug.MyBlocks.Response](_ =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
          myKeys <- listKeys()
          blockNum = historyDebug.count(b => myKeys.map(_.evidence).contains(b.generatorBox.evidence))
        } yield ToplRpc.Debug.MyBlocks.Response(myKeys, blockNum)
      )

    override val generators: ToplRpc.Debug.Generators.rpc.ServerHandler =
      Kleisli[Rpc.ServerResponse, ToplRpc.Debug.Generators.Params, ToplRpc.Debug.Generators.Response](_ =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
        } yield historyDebug.forgerDistribution().map { case (key, count) =>
          key.address -> count
        }
      )

    override val idsFromHeight: ToplRpc.Debug.IdsFromHeight.rpc.ServerHandler =
      Kleisli[Rpc.ServerResponse, ToplRpc.Debug.IdsFromHeight.Params, ToplRpc.Debug.IdsFromHeight.Response](params =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
          ids <- EitherT
            .fromOption[Future]
            .apply[RpcError[_], Seq[ModifierId]](
              historyDebug.getIdsFrom(params.height, params.limit),
              ToplRpcErrors.NoBlockIdsAtHeight: RpcError[_]
            )
        } yield ids
      )
  }

  class Utils(appContext: AppContext)(implicit throwableEncoder: Encoder[ThrowableData], ec: ExecutionContext)
      extends BifrostRpcHandlers.Utils {
    import Utils._

    val defaultSeedSize = 32 // todo: JAA - read this from a more appropriate place. Bip39 spec or something?

    override val seed: ToplRpc.Utils.Seed.rpc.ServerHandler =
      Kleisli[Rpc.ServerResponse, ToplRpc.Utils.Seed.Params, ToplRpc.Utils.Seed.Response](_ =>
        EitherT.pure[Future, RpcError[_]](ToplRpc.Utils.Seed.Response(generateSeed(defaultSeedSize)))
      )

    override val seedOfLength: ToplRpc.Utils.SeedOfLength.rpc.ServerHandler =
      Kleisli[Rpc.ServerResponse, ToplRpc.Utils.SeedOfLength.Params, ToplRpc.Utils.SeedOfLength.Response](params =>
        EitherT.pure[Future, RpcError[_]](ToplRpc.Utils.SeedOfLength.Response(generateSeed(params.length)))
      )

    override val hashBlake2b256: ToplRpc.Utils.HashBlake2b256.rpc.ServerHandler =
      Kleisli[Rpc.ServerResponse, ToplRpc.Utils.HashBlake2b256.Params, ToplRpc.Utils.HashBlake2b256.Response](params =>
        EitherT.pure[Future, RpcError[_]](
          ToplRpc.Utils.HashBlake2b256.Response(params.message, Base58.encode(Blake2b256(params.message)))
        )
      )

    override val generateAssetCode: ToplRpc.Utils.GenerateAssetCode.rpc.ServerHandler =
      Kleisli[Rpc.ServerResponse, ToplRpc.Utils.GenerateAssetCode.Params, ToplRpc.Utils.GenerateAssetCode.Response](
        params =>
          EitherT.fromEither[Future](
            Try(AssetCode(params.version, params.issuer, params.shortName)).toEither
              .leftMap(ToplRpcErrors.FailedToGenerateAssetCode)
              .map(ToplRpc.Utils.GenerateAssetCode.Response)
          )
      )

    override val checkValidAddress: ToplRpc.Utils.CheckValidAddress.rpc.ServerHandler =
      Kleisli[Rpc.ServerResponse, ToplRpc.Utils.CheckValidAddress.Params, ToplRpc.Utils.CheckValidAddress.Response](
        params =>
          EitherT.fromEither[Future](
            params.network match {
              case None =>
                NetworkType
                  .pickNetworkType(appContext.networkType.netPrefix)
                  .toRight(ToplRpcErrors.InvalidNetworkSpecified)
                  .map(nt => ToplRpc.Utils.CheckValidAddress.Response(params.address, nt.verboseName))
              case Some(networkName) =>
                NetworkType
                  .pickNetworkType(networkName)
                  .toRight(ToplRpcErrors.InvalidNetworkSpecified)
                  // TODO: Re-extract the `address` field using the new network type
                  .map(nt => ToplRpc.Utils.CheckValidAddress.Response(params.address, nt.verboseName))
            }
          )
      )
  }

  object Utils {

    private def generateSeed(length: Int): String = {
      val seed = new Array[Byte](length)
      new SecureRandom().nextBytes(seed) //seed mutated here!
      Base58.encode(seed)
    }
  }
}
