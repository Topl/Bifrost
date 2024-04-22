package co.topl.networking.blockchain

import com.google.common.primitives.Longs
import com.google.protobuf.ByteString

object NetworkProtocolVersions {

  val V0: ByteString = ByteString.copyFrom(Longs.toByteArray(0))
  val V1: ByteString = ByteString.copyFrom(Longs.toByteArray(1))

  val Local: ByteString = V1

}
