package co.topl.codecs.binary.scodecs.modifier.block

import co.topl.codecs.binary.scodecs.attestation._
import co.topl.codecs.binary.scodecs.crypto._
import co.topl.codecs.binary.scodecs.modifier.box._
import co.topl.codecs.binary.scodecs.modifier.transaction.transactionCodec
import co.topl.codecs.binary.scodecs.valuetypes._
import co.topl.modifier.ModifierId
import co.topl.modifier.block.{Block, BlockBody, BlockHeader, BloomFilter}
import co.topl.modifier.transaction.Transaction
import scodec.Codec
import shapeless.{::, HList, HNil}

import scala.reflect.ClassTag

trait BlockCodecs {
  implicit val modifierIdCodec: Codec[ModifierId] = bytesCodec(ModifierId.size).as[ModifierId]

  implicit def bloomFilterCodec(implicit longClassTag: ClassTag[Long]): Codec[BloomFilter] =
    sizedArrayCodec[Long](BloomFilter.numLongs)(longCodec, longClassTag).as[BloomFilter]

  implicit val blockCodec: Codec[Block] =
    (byteCodec ::
      modifierIdCodec ::
      uLongCodec ::
      arbitBoxCodec ::
      publicKeyPropositionCurve25519Codec ::
      signatureCurve25519Codec ::
      uLongCodec ::
      uLongCodec ::
      seqCodec(transactionCodec))
      .xmapc[Block] {
        case version ::
            modifierId ::
            timestamp ::
            arbitBox ::
            publicKey ::
            signature ::
            height ::
            difficulty ::
            txs ::
            HNil =>
          Block(modifierId, timestamp, arbitBox, publicKey, signature, height, difficulty, txs, version)
      }(block =>
        HList(
          block.version,
          block.parentId,
          block.timestamp,
          block.generatorBox,
          block.publicKey,
          block.signature,
          block.height,
          block.difficulty,
          block.transactions
        )
      )

  implicit val blockHeaderCodec: Codec[BlockHeader] =
    (byteCodec ::
      modifierIdCodec ::
      modifierIdCodec ::
      uLongCodec ::
      arbitBoxCodec ::
      publicKeyPropositionCurve25519Codec ::
      signatureCurve25519Codec ::
      longCodec ::
      longCodec ::
      digest32Codec ::
      bloomFilterCodec)
      .xmapc {
        case version ::
            blockId ::
            parentId ::
            timestamp ::
            generator ::
            publicKey ::
            signature ::
            height ::
            difficulty ::
            txRoot ::
            bloomFilter ::
            HNil =>
          BlockHeader(
            blockId,
            parentId,
            timestamp,
            generator,
            publicKey,
            signature,
            height,
            difficulty,
            txRoot,
            bloomFilter,
            version
          )
      } { blockHeader =>
        HList(
          blockHeader.version,
          blockHeader.id,
          blockHeader.parentId,
          blockHeader.timestamp,
          blockHeader.generatorBox,
          blockHeader.publicKey,
          blockHeader.signature,
          blockHeader.height,
          blockHeader.difficulty,
          blockHeader.txRoot,
          blockHeader.bloomFilter
        )
      }
      .as[BlockHeader]

  implicit val blockBodyCodec: Codec[BlockBody] =
    (byteCodec :: modifierIdCodec :: modifierIdCodec :: seqCodec(transactionCodec))
      .xmapc { case version :: blockId :: parentId :: txs :: HNil =>
        BlockBody(blockId, parentId, txs, version)
      }(blockBody => HList(blockBody.version, blockBody.id, blockBody.parentId, blockBody.transactions))
}
