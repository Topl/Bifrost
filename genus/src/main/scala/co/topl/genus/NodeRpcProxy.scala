package co.topl.genus

import cats.effect.Async
import cats.effect.kernel.Resource
import co.topl.grpc.makeChannel
import co.topl.node.services._
import io.grpc.Metadata

class NodeRpcProxy[F[_], Ctx](client: NodeRpcFs2Grpc[F, Ctx]) extends NodeRpcFs2Grpc[F, Ctx] {

  override def broadcastTransaction(request: BroadcastTransactionReq, ctx: Ctx): F[BroadcastTransactionRes] =
    client.broadcastTransaction(request, ctx)

  override def currentMempool(request: CurrentMempoolReq, ctx: Ctx): F[CurrentMempoolRes] =
    client.currentMempool(request, ctx)

  override def currentMempoolContains(request: CurrentMempoolContainsReq, ctx: Ctx): F[CurrentMempoolContainsRes] =
    client.currentMempoolContains(request, ctx)

  override def fetchBlockHeader(request: FetchBlockHeaderReq, ctx: Ctx): F[FetchBlockHeaderRes] =
    client.fetchBlockHeader(request, ctx)

  override def fetchBlockBody(request: FetchBlockBodyReq, ctx: Ctx): F[FetchBlockBodyRes] =
    client.fetchBlockBody(request, ctx)

  override def fetchTransaction(request: FetchTransactionReq, ctx: Ctx): F[FetchTransactionRes] =
    client.fetchTransaction(request, ctx)

  override def fetchBlockIdAtHeight(request: FetchBlockIdAtHeightReq, ctx: Ctx): F[FetchBlockIdAtHeightRes] =
    client.fetchBlockIdAtHeight(request, ctx)

  override def fetchBlockIdAtDepth(request: FetchBlockIdAtDepthReq, ctx: Ctx): F[FetchBlockIdAtDepthRes] =
    client.fetchBlockIdAtDepth(request, ctx)

  override def synchronizationTraversal(
    request: SynchronizationTraversalReq,
    ctx:     Ctx
  ): fs2.Stream[F, SynchronizationTraversalRes] =
    client.synchronizationTraversal(request, ctx)

  override def fetchNodeConfig(request: FetchNodeConfigReq, ctx: Ctx): fs2.Stream[F, FetchNodeConfigRes] =
    client.fetchNodeConfig(request, ctx)

  override def fetchEpochData(request: FetchEpochDataReq, ctx: Ctx): F[FetchEpochDataRes] =
    client.fetchEpochData(request, ctx)
}

object NodeRpcProxy {

  def make[F[_]: Async](nodeRpcHost: String, nodeRpcPort: Int, tls: Boolean): Resource[F, NodeRpcProxy[F, Metadata]] =
    makeChannel[F](nodeRpcHost, nodeRpcPort, tls)
      .flatMap(NodeRpcFs2Grpc.stubResource[F])
      .map(new NodeRpcProxy[F, Metadata](_))
}
