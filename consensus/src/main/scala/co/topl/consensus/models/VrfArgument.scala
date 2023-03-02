package co.topl.consensus.models

import co.topl.codecs.bytes.typeclasses.Signable
import co.topl.models._
import com.google.protobuf.ByteString

case class VrfArgument(eta: Eta, slot: Slot)

object VrfArgument {

  implicit val signableVrfArgument: Signable[VrfArgument] =
    arg => arg.eta.data.concat(ByteString.copyFrom(BigInt(arg.slot).toByteArray))
}
