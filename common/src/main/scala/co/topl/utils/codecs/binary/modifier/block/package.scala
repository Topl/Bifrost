package co.topl.utils.codecs.binary.modifier

import co.topl.modifier.ModifierId
import co.topl.modifier.block.{Block, BlockBody, BlockHeader, BloomFilter}
import co.topl.modifier.transaction.Transaction
import co.topl.utils.codecs.binary.attestation.codecs._
import co.topl.utils.codecs.binary.crypto.codecs._
import co.topl.utils.codecs.binary.modifier.box.codecs._
import co.topl.utils.codecs.binary.modifier.transaction.codecs._
import co.topl.utils.codecs.binary.valuetypes.implicits._
import scodec.Codec
import shapeless._
import spire.ClassTag

package object block {

  trait Codecs {

    implicit val modifierIdCodec: Codec[ModifierId] = ModifierIdCodec.codec

    implicit def bloomFilterCodec(implicit longClassTag: ClassTag[Long]): Codec[BloomFilter] =
      staticArrayCodec[Long](BloomFilter.numLongs)(longCodec, longClassTag).as[BloomFilter]

    implicit val blockCodec: Codec[Block] =
      (byteCodec ::
        modifierIdCodec ::
        uLongCodec ::
        arbitBoxCodec ::
        publicKeyPropositionCurve25519Codec ::
        signatureCurve25519Codec ::
        uLongCodec ::
        uLongCodec ::
        listCodec(transactionCodec).as[Seq[Transaction.TX]])
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
            block.id,
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

    implicit val blockBodySerializer: Codec[BlockBody] =
      (byteCodec :: modifierIdCodec :: modifierIdCodec :: listCodec(transactionCodec).as[Seq[Transaction.TX]])
        .xmapc { case version :: blockId :: parentId :: txs :: HNil =>
          BlockBody(blockId, parentId, txs, version)
        }(blockBody => HList(blockBody.version, blockBody.id, blockBody.parentId, blockBody.transactions))
  }

  object codecs extends Codecs
}
