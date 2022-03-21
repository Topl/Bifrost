package co.topl.demo

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{BroadcastHub, Keep, RunnableGraph, Sink, Source}
import cats.data.{EitherT, OptionT, Validated}
import cats.effect._
import cats.implicits._
import cats.{~>, Monad, MonadThrow, Show}
import co.topl.algebras.{Store, UnsafeResource}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.{BlockHeaderV2Ops, BlockHeaderValidationFailure}
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.PerpetualBlockMintAlgebra
import co.topl.models._
import co.topl.networking.p2p.{AkkaP2PServer, ConnectedPeer, ConnectionLeader, LocalPeer}
import co.topl.networking.{BlockchainProtocolHandlers, MultiplexedTypedPeerHandler}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import java.net.InetSocketAddress
import scala.concurrent.Future
import scala.concurrent.duration._

object DemoProgram {

  /**
   * A forever-running program which traverses epochs and the slots within the epochs
   */
  def run[F[_]: MonadThrow: Logger: Async: *[_] ~> Future](
    mints:              List[PerpetualBlockMintAlgebra[F]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    blockStore:         Store[F, BlockV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    bindPort:           Int,
    remotePeers:        Source[InetSocketAddress, _]
  )(implicit system:    ActorSystem[_]): F[Unit] =
    for {
      (p2pServer, onAdoptCallback) <- networking(bindPort, remotePeers)
//      onAdoptCallback   <- ((_: TypedIdentifier) => Applicative[F].unit).pure[F]
      mintedBlockStream <- mints.foldMapM(_.blocks)
      streamCompletionFuture = implicitly[RunnableGraph ~> F].apply(
        mintedBlockStream
          .mapAsyncF(1)(
            processMintedBlock[F](_, headerValidation, blockStore, localChain, ed25519VrfResource, onAdoptCallback)
          )
          .toMat(Sink.ignore)(Keep.right)
      )
      _ <- Async[F]
        .fromFuture(streamCompletionFuture)
        .void
    } yield ()

  implicit private val showBlockHeaderValidationFailure: Show[BlockHeaderValidationFailure] =
    Show.fromToString

  /**
   * Insert block to local storage and perform chain selection.  If better, validate the block and then adopt it locally.
   */
  private def processMintedBlock[F[_]: MonadThrow: Sync: Logger](
    nextBlock:          BlockV2,
    headerValidation:   BlockHeaderValidationAlgebra[F],
    blockStore:         Store[F, BlockV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    onAdopt:            TypedIdentifier => F[Unit]
  ): F[Unit] =
    for {
      _                     <- Logger[F].info(show"Minted block ${nextBlock.headerV2}")
      _                     <- blockStore.put(nextBlock.headerV2.id, nextBlock)
      slotData              <- ed25519VrfResource.use(implicit ed25519Vrf => nextBlock.headerV2.slotData.pure[F])
      localChainIsWorseThan <- localChain.isWorseThan(slotData)
      _ <- Monad[F].ifElseM(
        localChainIsWorseThan.pure[F] ->
        Sync[F].defer(
          EitherT(
            OptionT(blockStore.get(nextBlock.headerV2.parentHeaderId))
              .getOrElseF(MonadThrow[F].raiseError(new NoSuchElementException(nextBlock.headerV2.parentHeaderId.show)))
              .flatMap(parent => headerValidation.validate(nextBlock.headerV2, parent.headerV2))
          )
            // TODO: Now fetch the body from the network and validate against the ledger
            .semiflatTap(_ => localChain.adopt(Validated.Valid(slotData)))
            .semiflatTap(header => onAdopt(header.id))
            .semiflatTap(header => Logger[F].info(show"Adopted local head block id=${header.id}"))
            .void
            .valueOrF(e =>
              Logger[F]
                .warn(show"Invalid block header. reason=$e block=${nextBlock.headerV2}")
                // TODO: Penalize the peer
                .flatTap(_ => blockStore.remove(nextBlock.headerV2.id))
            )
        )
      )(
        Logger[F].info(show"Ignoring weaker block header id=${nextBlock.headerV2.id}")
      )
    } yield ()

  private def networking[F[_]: Async: Logger: *[_] ~> Future](bindPort: Int, remotePeers: Source[InetSocketAddress, _])(
    implicit system:                                                    ActorSystem[_]
  ) =
    for {
      (handlers: BlockchainProtocolHandlers[F], onAdoptCallback: (TypedIdentifier => F[Unit])) <- protocolHandlers[F]()
      localAddress = InetSocketAddress.createUnresolved("localhost", bindPort)
      multiplexedPeerHandler: MultiplexedTypedPeerHandler[F] = (
        connectedPeer: ConnectedPeer,
        leader:        ConnectionLeader
      ) =>
        BlockchainProtocolHandlers
          .standardProtocolSet[F](handlers, connectedPeer, leader, LocalPeer(localAddress))
          .map(Source.single(_).concat(Source.never))
      p2pServer <- {
        implicit val classicSystem = system.classicSystem
        import classicSystem.dispatcher
        AkkaP2PServer.make(
          "localhost",
          bindPort,
          localAddress,
          Source
            .future(akka.pattern.after(1.seconds, classicSystem.scheduler)(Future.unit))
            .flatMapConcat(_ => remotePeers),
          (connectedPeer, leader) => multiplexedPeerHandler.multiplexed(connectedPeer, leader)
        )
      }
      _ <- Logger[F].info(s"Bound P2P at host=localhost port=$bindPort")
    } yield (p2pServer, onAdoptCallback)

  private def protocolHandlers[F[_]: Sync: Logger: *[_] ~> Future]()(implicit
    mat: Materializer
  ): F[(BlockchainProtocolHandlers[F], TypedIdentifier => F[Unit])] =
    Sync[F].delay {

      val (queue, source) = Source.queue[TypedIdentifier](128).toMat(BroadcastHub.sink)(Keep.both).run()

      val handlers =
        new BlockchainProtocolHandlers[F] {
          def blockAdoptionNotificationClientSink(connectedPeer: ConnectedPeer): F[Sink[TypedIdentifier, NotUsed]] =
            Sink
              .foreachAsync[TypedIdentifier](1)(id =>
                implicitly[F ~> Future].apply(Logger[F].info(show"Received remote blockId=$id"))
              )
              .mapMaterializedValue(_ => NotUsed: NotUsed)
              .pure[F]

          def blockAdoptionNotificationServerSource(connectedPeer: ConnectedPeer): F[Source[TypedIdentifier, NotUsed]] =
            source
              .alsoTo(
                Sink.foreachAsync(1)(id =>
                  implicitly[F ~> Future].apply(Logger[F].info(show"Sending local blockId=$id"))
                )
              )
              .pure[F]

        }

      handlers -> (id => (queue.offer(id)).pure[F].void)
    }

}
