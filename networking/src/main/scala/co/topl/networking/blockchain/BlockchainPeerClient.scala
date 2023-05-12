package co.topl.networking.blockchain

import cats._
import cats.data.OptionT
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.models.utility.Ratio
import co.topl.node.models.{BlockBody, CurrentKnownHostsReq, CurrentKnownHostsRes}
import co.topl.networking.p2p.ConnectedPeer
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger

/**
 * A client which can be used by a local node to retrieve blockchain data from a remote peer
 */
trait BlockchainPeerClient[F[_]] {

  /**
   * The ConnectedPeer for this current connection
   */
  def remotePeer: F[ConnectedPeer]

  /**
   * A Source of block IDs that were adopted by the remote node
   */
  def remotePeerAdoptions: F[Stream[F, BlockId]]

  /**
   * A Source of transaction IDs that were observed by the remote node
   */
  def remoteTransactionNotifications: F[Stream[F, TransactionId]]

  /**
   * A lookup to retrieve a remote block at depth, where depth=0 is the best tip, and depth=1 is the parent of best tip
   */
  def getRemoteBlockIdAtDepth(depth: Long): F[Option[BlockId]]

  /**
   * A lookup to retrieve a remote best block id
   */
  def remoteCurrentTip(): F[Option[BlockId]] = getRemoteBlockIdAtDepth(0)

  /**
   * A Lookup to retrieve a remote SlotData by ID
   */
  def getRemoteSlotData(id: BlockId): F[Option[SlotData]]

  /**
   * A Lookup to retrieve a remote slot data by ID, or throw specified Error
   */
  def getSlotDataOrError[E <: Throwable](id: BlockId, error: => E)(implicit MonadThrow: MonadThrow[F]): F[SlotData] =
    OptionT(getRemoteSlotData(id)).getOrRaise(error)

  /**
   * A Lookup to retrieve a remote block header by ID
   */
  def getRemoteHeader(id: BlockId): F[Option[BlockHeader]]

  /**
   * A Lookup to retrieve a remote block header by ID, or throw specified Error
   */
  def getHeaderOrError[E <: Throwable](id: BlockId, error: => E)(implicit MonadThrow: MonadThrow[F]): F[BlockHeader] =
    OptionT(getRemoteHeader(id)).getOrRaise(error)

  /**
   * A Lookup to retrieve a remote block body by ID
   */
  def getRemoteBody(id: BlockId): F[Option[BlockBody]]

  /**
   * A lookup to retrieve a remote transaction by ID
   */
  def getRemoteTransaction(id: TransactionId): F[Option[IoTransaction]]

  /**
   * A lookup to retrieve the remote node's block ID associated with the given height.
   * @param height The height to lookup
   * @param localBlockId The block ID of the local node at the requested height (the remote peer can cache this to avoid
   *                     an extra lookup from their end)
   */
  def getRemoteBlockIdAtHeight(height: Long, localBlockId: Option[BlockId]): F[Option[BlockId]]

  /**
   * A lookup to retrieve known hosts from remote peer
   * @param request request for known hosts
   * @return known hosts
   */
  def getRemoteKnownHosts(request: CurrentKnownHostsReq): F[Option[CurrentKnownHostsRes]]

  /**
   * Find the common ancestor block ID between the local node and the remote peer.
   */
  def findCommonAncestor(
    getLocalBlockIdAtHeight: Long => F[BlockId],
    currentHeight:           () => F[Long]
  )(implicit syncF: Sync[F], loggerF: Logger[F]): F[BlockId] =
    Sync[F]
      .defer(
        for {
          initialHeight <- currentHeight()
          intersectionOpt <- narySearch(getLocalBlockIdAtHeight, getRemoteBlockIdAtHeight, Ratio(2, 3))
            .apply(1L, initialHeight)
          intersection <- OptionT
            .fromOption[F](intersectionOpt)
            .getOrElseF(
              MonadThrow[F].raiseError(new IllegalStateException("Unable to find common ancestor with remote peer"))
            )
        } yield intersection
      )

  /**
   * A "divide-and-conquer" search, similar to binary search, but the "array" is divided into potentially unequal parts.
   * Binary search cuts the search space in half, whereas this search cuts the search space into sizes determined
   * by the given `searchSpaceTarget`.
   *
   * In cases where you expect to find the value closer to the "end" of the "array", a larger `searchSpaceTarget` will
   * converge on a result faster.
   * TODO: Could this be a "function" that "accelerates"?  Meaning, if the search space recurses to the right several
   * times, could the search detect that and increase the searchSpaceTarget ratio to be more aggressive?
   */
  private def narySearch[T: Eq](
    getLocal:          Long => F[T],
    getRemote:         (Long, Option[T]) => F[Option[T]],
    searchSpaceTarget: Ratio
  )(implicit monadF: Monad[F], loggerF: Logger[F]): (Long, Long) => F[Option[T]] = {
    lazy val f: (Long, Long, Option[T]) => F[Option[T]] = (min, max, ifNone) =>
      Logger[F].debug(show"Recursing common ancestor search in bounds=($min, $max)") >>
      (min === max)
        .pure[F]
        .ifM(
          ifTrue = getLocal(min)
            .flatMap(localValue =>
              OptionT(getRemote(min, localValue.some))
                .filter(_ === localValue)
                .orElse(OptionT.fromOption[F](ifNone))
                .value
            ),
          ifFalse = for {
            targetHeight <- (min + ((max - min) * searchSpaceTarget.toDouble).floor.round).pure[F]
            localValue   <- getLocal(targetHeight)
            remoteValue  <- getRemote(targetHeight, localValue.some)
            result <- remoteValue
              .filter(_ === localValue)
              .fold(f(min, targetHeight, ifNone))(remoteValue => f(targetHeight + 1, max, remoteValue.some))
          } yield result
        )
    (min, max) => f(min, max, None)
  }

}
