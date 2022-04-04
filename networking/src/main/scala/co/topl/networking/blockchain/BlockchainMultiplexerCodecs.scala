package co.topl.networking.blockchain

import co.topl.codecs.bytes.scodecs._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.models.TypedIdentifier

object BlockchainMultiplexerCodecs {

  implicit val longTypedIdentifierOptTransmittable: Transmittable[(Long, Option[TypedIdentifier])] =
    Transmittable.instanceFromCodec(
      (longCodec :: optionCodec[TypedIdentifier])
    )

}
