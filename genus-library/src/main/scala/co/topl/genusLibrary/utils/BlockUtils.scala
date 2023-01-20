package co.topl.genusLibrary.utils

import co.topl.codecs.bytes.tetra.{TetraIdentifiableInstances, TetraScodecCodecs}
import co.topl.genusLibrary.GenusException
import co.topl.models.{BlockBody, BlockHeader, TypePrefix}
import scodec.Codec

trait BlockUtils {

  def getParentBlockId(header: BlockHeader): Array[Byte] = header.parentHeaderId.allBytes.toArray

  def getBlockId(header: BlockHeader): Array[Byte] = {
    val (typePrefix, bytes) = TetraIdentifiableInstances.identifiableBlockHeader.idOf(header)
    typedBytesTupleToByteArray((typePrefix, bytes.toArray))
  }

  def blockBodyToByteArray(blockBody: BlockBody): Array[Byte] =
    encodeToByteArray(blockBody, TetraScodecCodecs.blockBodyCodec, "BlockBody")

  def typedBytesTupleToByteArray(id: (TypePrefix, Array[Byte])): Array[Byte] = {
    val a: Array[Byte] = new Array(1 + id._2.length)
    a(0) = id._1
    Array.copy(id._2, 0, a, 1, id._2.length)
    a
  }

  protected def encodeToByteArray[T <: java.io.Serializable](
    scalaObject: T,
    codec:       Codec[T],
    typeName:    String
  ): Array[Byte] =
    codec
      .encode(scalaObject)
      .mapErr(err => throw GenusException(s"Error encoding $typeName: ${err.messageWithContext}"))
      .require
      .toByteArray

}
