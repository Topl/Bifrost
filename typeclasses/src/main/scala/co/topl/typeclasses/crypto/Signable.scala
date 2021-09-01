package co.topl.typeclasses.crypto

import co.topl.models.{BlockHeaderV2, Bytes, PolyTransfer}
import simulacrum.{op, typeclass}

@typeclass trait Signable[T] {
  @op("signableBytes") def signableBytesOf(t: T): Bytes
}

object Signable {

  trait Instances {

    implicit val polyTransfer: Signable[PolyTransfer] =
      _ => Bytes(Array.fill[Byte](1024)(0))

    implicit val blockHeaderV2: Signable[BlockHeaderV2] =
      _ => Bytes(Array.fill[Byte](1024)(0))
    implicit val byteArray: Signable[Array[Byte]] = Bytes(_)
  }
  object Instances extends Instances
}
