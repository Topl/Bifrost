package co.topl.typeclasses.crypto

import co.topl.codecs.bytes.BasicCodecs._
import co.topl.codecs.bytes.ByteCodec.implicits._
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
      header =>
        header.parentHeaderId.allBytes ++ header.txRoot.data ++ header.bloomFilter.data ++ Bytes(
          BigInt(header.timestamp).toByteArray
        ) ++
        Bytes(BigInt(header.height).toByteArray) ++
        Bytes(BigInt(header.slot).toByteArray) ++
        header.vrfCertificate.bytes ++
        Bytes(header.metadata.fold(Array.emptyByteArray)(_.data.value)) ++
        header.address.bytes

    implicit val byteArray: Signable[Array[Byte]] = Bytes(_)
  }
  object Instances extends Instances
}
