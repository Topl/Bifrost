package co.topl.networking.blockchain

import co.topl.codecs.bytes.tetra.instances._
import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}
import co.topl.networking.blockchain.NetworkTypeTags._
import co.topl.networking.multiplexer.MultiplexerCodecBuilder
import co.topl.networking.multiplexer.MultiplexerCodecs._
import co.topl.networking.typedprotocols.TypedProtocol

object BlockchainMultiplexerCodecs {

  val multiplexerCodec =
    MultiplexerCodecBuilder(Map.empty, Map.empty)
      .withCodec[TypedProtocol.CommonMessages.Start.type](1: Byte)
      .withCodec[TypedProtocol.CommonMessages.Done.type](2: Byte)
      .withCodec[TypedProtocol.CommonMessages.Get[TypedIdentifier]](3: Byte)
      //      .withCodec[TypedProtocol.CommonMessages.Response[SlotData]](4: Byte)
      .withCodec[TypedProtocol.CommonMessages.Response[BlockHeaderV2]](5: Byte)
      .withCodec[TypedProtocol.CommonMessages.Response[BlockBodyV2]](6: Byte)
      .withCodec[TypedProtocol.CommonMessages.Response[Transaction]](7: Byte)
      .withCodec[TypedProtocol.CommonMessages.Push[TypedIdentifier]](8: Byte)
      .multiplexerCodec

}
