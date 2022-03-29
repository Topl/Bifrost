package co.topl.networking.blockchain

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.{Keep, Sink}
import cats.MonadThrow
import cats.data.OptionT
import cats.effect.kernel.Sync
import cats.effect.{Async, Concurrent}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

trait BlockchainClientHandler[F[_]] {
  def useClient(client: BlockchainPeerClient[F]): F[Unit]
}

object BlockchainClientHandler {

  object FetchAllBlocks {

    def make[F[_]: Async: Concurrent: Logger: FToFuture](
      headerStore:      Store[F, BlockHeaderV2],
      bodyStore:        Store[F, BlockBodyV2],
      transactionStore: Store[F, Transaction],
      onBlockReceived:  BlockV2 => F[Unit]
    )(implicit system:  ActorSystem[_]): F[BlockchainClientHandler[F]] =
      Sync[F].delay(
        (
          client =>
            for {
              remoteAdoptionsSource <- client.remotePeerAdoptions
              _ <- Async[F].fromFuture(
                remoteAdoptionsSource
                  .mapAsyncF(1)(
                    processRemoteBlockNotification[F](
                      client,
                      headerStore,
                      bodyStore,
                      transactionStore,
                      onBlockReceived
                    )
                  )
                  .toMat(Sink.ignore)(Keep.right)
                  .liftTo[F]
              )
            } yield ()
        ): BlockchainClientHandler[F]
      )

    private def processRemoteBlockNotification[F[_]: Async: Concurrent: Logger: FToFuture](
      client:           BlockchainPeerClient[F],
      headerStore:      Store[F, BlockHeaderV2],
      bodyStore:        Store[F, BlockBodyV2],
      transactionStore: Store[F, Transaction],
      onBlockReceived:  BlockV2 => F[Unit]
    )(id:               TypedIdentifier) =
      for {
        _                  <- Logger[F].info(show"Remote peer adopted block id=$id")
        maybeCurrentHeader <- headerStore.get(id)
        _ <- (id, maybeCurrentHeader)
          // Recursively fetch the remote header+body+transactions until a common ancestor is found
          .iterateWhileM[F] { case (id, _) =>
            fetchHeader(client, headerStore)(id)
              .productL(fetchBody(client, bodyStore)(id).flatMap(fetchTransactions(client, transactionStore)).void)
              .flatMap(header => headerStore.get(header.parentHeaderId).tupleLeft(header.parentHeaderId))
          }(_._2.isEmpty)
        _ <- OptionT
          .fromOption[F](maybeCurrentHeader)
          .flatTapNone(
            (OptionT(headerStore.get(id)), OptionT(bodyStore.get(id)))
              .mapN((header, body) => BlockV2(header, body))
              .semiflatTap(_ => Logger[F].info(show"Processing remote block id=$id"))
              .semiflatTap(onBlockReceived)
              .value
          )
          .value
      } yield ()

    private def fetchHeader[F[_]: Async: Concurrent: Logger: FToFuture](
      client:      BlockchainPeerClient[F],
      headerStore: Store[F, BlockHeaderV2]
    )(id:          TypedIdentifier) =
      OptionT(headerStore.get(id))
        .orElse(
          OptionT
            .liftF[F, Unit](Sync[F].defer(Logger[F].info(show"Requesting header id=$id")))
            .flatMap(_ =>
              OptionT(client.getRemoteHeader(id))
                // TODO:  Verify the locally computed header ID against `id`
                .semiflatTap(header => Logger[F].info(show"Inserting remote header id=$id"))
                .flatTapNone(Logger[F].info(show"Remote did not possess header id=$id"))
                .semiflatTap(header => headerStore.put(header.id, header))
            )
        )
        .getOrNoSuchElement(id.show)

    private def fetchBody[F[_]: Async: Concurrent: Logger: FToFuture](
      client:    BlockchainPeerClient[F],
      bodyStore: Store[F, BlockBodyV2]
    )(id:        TypedIdentifier) =
      OptionT(bodyStore.get(id))
        .orElse(
          OptionT
            .liftF[F, Unit](Sync[F].defer(Logger[F].info(show"Requesting body id=$id")))
            .flatMap(_ =>
              OptionT(client.getRemoteBody(id))
                // TODO: Verify the transaction IDs associated with this body match the txRoot of the header
                .semiflatTap(_ => Logger[F].info(show"Inserting remote body id=$id"))
                .flatTapNone(Logger[F].info(show"Remote did not possess body id=$id"))
                .semiflatTap(bodyStore.put(id, _))
            )
        )
        .getOrNoSuchElement(id.show)

    private def fetchTransactions[F[_]: Async: Concurrent: Logger: FToFuture](
      client:           BlockchainPeerClient[F],
      transactionStore: Store[F, Transaction]
    ) =
      (body: BlockBodyV2) => body.traverse(fetchTransaction[F](client, transactionStore))

    private def fetchTransaction[F[_]: Async: Concurrent: Logger: FToFuture](
      client:           BlockchainPeerClient[F],
      transactionStore: Store[F, Transaction]
    ) = { (id: TypedIdentifier) =>
      OptionT(transactionStore.get(id))
        .orElse(
          OptionT
            .liftF[F, Unit](Sync[F].defer(Logger[F].info(show"Requesting transaction id=$id"))) >>
          OptionT(client.getRemoteTransaction(id))
            // TODO:  Verify the locally computed transaction ID against `id`
            .semiflatTap(_ => Logger[F].info(show"Inserting remote transaction id=$id"))
            .flatTapNone(Logger[F].info(show"Remote did not possess transaction id=$id"))
            .semiflatTap(transactionStore.put(id, _))
        )
        .getOrNoSuchElement(id.show)
    }

    implicit class OptionTOps[F[_], T](optionT: OptionT[F, T]) {

      def getOrNoSuchElement(id: Any)(implicit M: MonadThrow[F]): F[T] =
        optionT.getOrElseF(M.raiseError(new NoSuchElementException(id.toString)))
    }
  }
}
