package co.topl.protobuf

import co.topl.protobuf.utility.{HasLength, Lengths, Sized}
import HasLength.instances.bytesLength

object Ed25519Sized {

  type Length = Lengths.`32`.type

  def verificationKeyEd25519(seed: Array[Byte]): Array[Byte] =
    Sized.strictUnsafe(Bytes(seed))(bytesLength, Lengths.`32`).data.toArray

  def proofEd25519(sig: Array[Byte]): Array[Byte] =
    Sized.strictUnsafe(Bytes(sig))(bytesLength, Lengths.`64`).data.toArray

}
