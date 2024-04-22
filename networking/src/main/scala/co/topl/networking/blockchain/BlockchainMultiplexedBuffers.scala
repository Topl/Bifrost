package co.topl.networking.blockchain

import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models._
import co.topl.networking.multiplexer.MultiplexedBuffer
import co.topl.node.models._

/**
 * Holds several instances of MultiplexedBuffers, specific to blockchain purposes
 */
case class BlockchainMultiplexedBuffers[F[_]](
  knownHosts:           MultiplexedBuffer[F, CurrentKnownHostsReq, Option[CurrentKnownHostsRes]],
  remotePeerServer:     MultiplexedBuffer[F, Unit, Option[KnownHost]],
  blockAdoptions:       MultiplexedBuffer[F, Unit, BlockId],
  transactionAdoptions: MultiplexedBuffer[F, Unit, TransactionId],
  pingPong:             MultiplexedBuffer[F, PingMessage, Option[PongMessage]],
  blockIdAtHeight:      MultiplexedBuffer[F, Long, Option[BlockId]],
  blockIdAtDepth:       MultiplexedBuffer[F, Long, Option[BlockId]],
  slotData:             MultiplexedBuffer[F, BlockId, Option[SlotData]],
  headers:              MultiplexedBuffer[F, BlockId, Option[BlockHeader]],
  bodies:               MultiplexedBuffer[F, BlockId, Option[BlockBody]],
  transactions:         MultiplexedBuffer[F, TransactionId, Option[IoTransaction]],
  appLevel:             MultiplexedBuffer[F, Boolean, Unit]
)

object BlockchainMultiplexedBuffers {

  def make[F[_]: Async]: Resource[F, BlockchainMultiplexedBuffers[F]] =
    (
      MultiplexedBuffer.make[F, CurrentKnownHostsReq, Option[CurrentKnownHostsRes]],
      MultiplexedBuffer.make[F, Unit, Option[KnownHost]],
      MultiplexedBuffer.make[F, Unit, BlockId],
      MultiplexedBuffer.make[F, Unit, TransactionId],
      MultiplexedBuffer.make[F, PingMessage, Option[PongMessage]],
      MultiplexedBuffer.make[F, Long, Option[BlockId]],
      MultiplexedBuffer.make[F, Long, Option[BlockId]],
      MultiplexedBuffer.make[F, BlockId, Option[SlotData]],
      MultiplexedBuffer.make[F, BlockId, Option[BlockHeader]],
      MultiplexedBuffer.make[F, BlockId, Option[BlockBody]],
      MultiplexedBuffer.make[F, TransactionId, Option[IoTransaction]],
      MultiplexedBuffer.make[F, Boolean, Unit]
    ).mapN(BlockchainMultiplexedBuffers.apply[F])

}
