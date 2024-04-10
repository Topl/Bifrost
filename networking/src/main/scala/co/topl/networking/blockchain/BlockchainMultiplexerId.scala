package co.topl.networking.blockchain

import cats.implicits._

sealed abstract class BlockchainMultiplexerId(val id: Int)

object BlockchainMultiplexerId {
  def parse(id: Int): Option[BlockchainMultiplexerId] =
    id match {
      case 10 => BlockIdAtHeightRequest.some
      case 11 => BlockIdAtDepthRequest.some
      case 12 => SlotDataRequest.some
      case 13 => HeaderRequest.some
      case 14 => BodyRequest.some
      case 15 => BlockAdoptionRequest.some
      case 16 => TransactionRequest.some
      case 17 => TransactionNotificationRequest.some
      case 18 => KnownHostsRequest.some
      case 19 => RemotePeerServerRequest.some
      case 20 => PingRequest.some
      case 21 => AppLevelRequest.some
    }

  case object BlockIdAtHeightRequest extends BlockchainMultiplexerId(10)
  case object BlockIdAtDepthRequest extends BlockchainMultiplexerId(11)
  case object SlotDataRequest extends BlockchainMultiplexerId(12)
  case object HeaderRequest extends BlockchainMultiplexerId(13)
  case object BodyRequest extends BlockchainMultiplexerId(14)
  case object BlockAdoptionRequest extends BlockchainMultiplexerId(15)
  case object TransactionRequest extends BlockchainMultiplexerId(16)
  case object TransactionNotificationRequest extends BlockchainMultiplexerId(17)
  case object KnownHostsRequest extends BlockchainMultiplexerId(18)
  case object RemotePeerServerRequest extends BlockchainMultiplexerId(19)
  case object PingRequest extends BlockchainMultiplexerId(20)
  case object AppLevelRequest extends BlockchainMultiplexerId(21)
}
