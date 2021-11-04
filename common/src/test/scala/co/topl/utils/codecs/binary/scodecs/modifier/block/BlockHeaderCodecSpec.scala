package co.topl.utils.codecs.binary.scodecs.modifier.block

import cats.implicits._
import cats.{Eq, Show}
import co.topl.modifier.block.BlockHeader
import co.topl.utils.CommonGenerators
import co.topl.utils.StringDataTypes.implicits._
import co.topl.utils.codecs.binary.{CodecCompatabilityBehavior, _}
import co.topl.utils.codecs.binary.legacy.modifier.block.BlockHeaderSerializer

class BlockHeaderCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[BlockHeader] =
    (a, b) =>
      a.id == b.id &&
      a.parentId == b.parentId &&
      a.timestamp == b.timestamp &&
      a.generatorBox == b.generatorBox &&
      a.publicKey.pubKeyBytes.value.sameElements(b.publicKey.pubKeyBytes.value) &&
      a.signature.sigBytes.value.sameElements(b.signature.sigBytes.value) &&
      a.height == b.height &&
      a.difficulty == b.difficulty &&
      a.txRoot.value.sameElements(b.txRoot.value) &&
      a.bloomFilter.value.sameElements(b.bloomFilter.value) &&
      a.version == b.version

  implicit private val show: Show[BlockHeader] =
    a =>
      s"BlockHeader(" +
      s"${a.id}, " +
      s"${a.parentId}, " +
      s"${a.timestamp}, " +
      s"${a.generatorBox}, " +
      s"${a.publicKey.pubKeyBytes.encodeAsBase58.show}, " +
      s"${a.signature.sigBytes.value.encodeAsBase58.show}, " +
      s"${a.height}, " +
      s"${a.difficulty}, ${a.txRoot.value.encodeAsBase58.show}, " +
      s"${a.bloomFilter}, " +
      s"${a.version}" +
      s")"

  codecCompatabilityBehavior(
    "block header",
    blockHeaderCodec,
    BlockHeaderSerializer,
    blockCurve25519Gen.map(_.toComponents._1)
  )
}
