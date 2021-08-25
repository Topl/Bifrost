package co.topl.typeclasses.crypto

import co.topl.models.{BlockHeaderV2, Bytes, PolyTransfer}
import simulacrum.{op, typeclass}

@typeclass trait Signable[T] {
  @op("signableBytes") def signableBytesOf(t: T): Bytes
}

object Signable {

  object Instances {
    implicit val polyTransfer: Signable[PolyTransfer] = ???
    implicit val blockHeaderV2: Signable[BlockHeaderV2] = ???
    implicit val byteArray: Signable[Array[Byte]] = Bytes(_)
  }
}

object Test extends App {
  import Signable.Instances._
  import Signable.ops._

  val polyTransfer: PolyTransfer = ???

  val bytes: Bytes = polyTransfer.signableBytes
}
