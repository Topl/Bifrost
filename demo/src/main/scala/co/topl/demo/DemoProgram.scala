package co.topl.demo

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Keep, RunnableGraph, Sink, Source}
import cats.data.{EitherT, OptionT, Validated}
import cats.effect._
import cats.implicits._
import cats.{~>, Monad, MonadError, MonadThrow, Parallel, Show}
import co.topl.algebras.{Store, UnsafeResource}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.{BlockHeaderV2Ops, BlockHeaderValidationFailure}
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.PerpetualBlockMintAlgebra
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.concurrent.Future

object DemoProgram {

  /**
   * A forever-running program which traverses epochs and the slots within the epochs
   */
  def run[F[_]: MonadError[*[_], Throwable]: Logger: Parallel: Clock: Async: *[_] ~> Future](
    mints:              List[PerpetualBlockMintAlgebra[F, Source[*, NotUsed]]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    blockStore:         Store[F, BlockV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF]
  )(implicit mat:       Materializer): F[Unit] =
    for {
      mintedBlockStream <- mints.foldMapM(_.blocks)
      _ <- Async[F]
        .fromFuture(
          implicitly[RunnableGraph ~> F].apply(
            mintedBlockStream
              .mapAsyncF(1)(processMintedBlock[F](_, headerValidation, blockStore, localChain, ed25519VrfResource))
              .toMat(Sink.ignore)(Keep.right)
          )
        )
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
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF]
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

}
