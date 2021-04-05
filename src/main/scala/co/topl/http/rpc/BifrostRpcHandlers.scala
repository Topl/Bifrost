package co.topl.http.rpc

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{CustomError, RpcError, ThrowableData}
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
  debug: BifrostRpcHandlers.Debug
)

object BifrostRpcHandlers {

  trait Debug {
    def delay: BifrostRpc.Debug.Delay.rpc.Handler
    def myBlocks: BifrostRpc.Debug.MyBlocks.rpc.Handler
    def generators: BifrostRpc.Debug.Generators.rpc.Handler
    def idsFromHeight: BifrostRpc.Debug.IdsFromHeight.rpc.Handler
  }

  trait Utils {
    def seed: BifrostRpc.Utils.Seed.rpc.Handler
    def seedOfLength: BifrostRpc.Utils.SeedOfLength.rpc.Handler
    def hashBlake2b256: BifrostRpc.Utils.HashBlake2b256.rpc.Handler
    def generateAssetCode: BifrostRpc.Utils.GenerateAssetCode.rpc.Handler
    def checkValidAddress: BifrostRpc.Utils.CheckValidAddress.rpc.Handler
  }
}

object BifrostRpcHandlerImpls {

  class Debug(appContext: AppContext, nodeViewHolderRef: ActorRef, forgerRef: ActorRef)(implicit
    ec:                   ExecutionContext,
    timeout:              Timeout,
    throwableEncoder:     Encoder[ThrowableData]
  ) extends BifrostRpcHandlers.Debug {

    import co.topl.consensus.Forger
    import co.topl.nodeView.NodeViewHolder

    implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

    type CV = CurrentView[History, State, MemPool]

    private def currentView(): EitherT[Future, RpcError[_], CV] =
      EitherT.liftF((nodeViewHolderRef ? NodeViewHolder.ReceivableMessages.GetDataFromCurrentView).mapTo[CV])

    private def listKeys(): EitherT[Future, RpcError[_], Set[Address]] =
      EitherT.liftF((forgerRef ? Forger.ReceivableMessages.ListKeys).mapTo[Set[Address]])

    override val delay: BifrostRpc.Debug.Delay.rpc.Handler =
      params =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
          delay <- EitherT.fromEither[Future](
            historyDebug
              .averageDelay(params.blockId, params.numBlocks)
              .toEither
              .leftMap(CustomError.fromThrowable(-32099, "Unexpected server error", _): RpcError[_])
          )
        } yield BifrostRpc.Debug.Delay.Response(s"$delay milliseconds")

    override val myBlocks: BifrostRpc.Debug.MyBlocks.rpc.Handler =
      _ =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
          myKeys <- listKeys()
          blockNum = historyDebug.count(b => myKeys.map(_.evidence).contains(b.generatorBox.evidence))
        } yield BifrostRpc.Debug.MyBlocks.Response(myKeys, blockNum)

    override val generators: BifrostRpc.Debug.Generators.rpc.Handler =
      _ =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
        } yield historyDebug.forgerDistribution().map { case (key, count) =>
          key.address -> count
        }

    override val idsFromHeight: BifrostRpc.Debug.IdsFromHeight.rpc.Handler =
      params =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
          ids <- EitherT
            .fromOption[Future]
            .apply[RpcError[_], Seq[ModifierId]](
              historyDebug.getIdsFrom(params.height, params.limit),
              BifrostRpcErrors.NoBlockIdsAtHeight: RpcError[_]
            )
        } yield ids
  }

  class Utils(appContext: AppContext)(implicit throwableEncoder: Encoder[ThrowableData], ec: ExecutionContext)
      extends BifrostRpcHandlers.Utils {
    import Utils._
    val defaultSeedSize = 32 // todo: JAA - read this from a more appropriate place. Bip39 spec or something?

    override val seed: BifrostRpc.Utils.Seed.rpc.Handler =
      _ => EitherT.pure[Future, RpcError[_]](BifrostRpc.Utils.Seed.Response(generateSeed(defaultSeedSize)))

    override val seedOfLength: BifrostRpc.Utils.SeedOfLength.rpc.Handler =
      params => EitherT.pure[Future, RpcError[_]](BifrostRpc.Utils.Seed.Response(generateSeed(params.length)))

    override val hashBlake2b256: BifrostRpc.Utils.HashBlake2b256.rpc.Handler =
      params =>
        EitherT.pure[Future, RpcError[_]](
          BifrostRpc.Utils.HashBlake2b256.Response(params.message, Base58.encode(Blake2b256(params.message)))
        )

    override val generateAssetCode: BifrostRpc.Utils.GenerateAssetCode.rpc.Handler =
      params =>
        EitherT.fromEither[Future](
          Try(AssetCode(params.version, params.issuer, params.shortName)).toEither
            .leftMap(BifrostRpcErrors.FailedToGenerateAssetCode)
        )

    override val checkValidAddress: BifrostRpc.Utils.CheckValidAddress.rpc.Handler =
      params =>
        EitherT.fromEither[Future](
          params.network match {
            case None =>
              NetworkType
                .pickNetworkType(appContext.networkType.netPrefix)
                .toRight(BifrostRpcErrors.InvalidNetworkSpecified)
                .map(nt => BifrostRpc.Utils.CheckValidAddress.Response(params.address, nt.verboseName))
            case Some(networkName) =>
              NetworkType
                .pickNetworkType(networkName)
                .toRight(BifrostRpcErrors.InvalidNetworkSpecified)
                // TODO: Re-extract the `address` field using the new network type
                .map(nt => BifrostRpc.Utils.CheckValidAddress.Response(params.address, nt.verboseName))
          }
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
