package examples.bifrost.blocks

import com.google.common.primitives.{Ints, Longs}
import examples.bifrost.transaction.{BifrostTransaction, BifrostTransactionCompanion}
import examples.curvepos.transaction.SimpleBlock._
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58

import scala.util.Try

case class BifrostBlock(override val parentId: BlockId,
                       override val timestamp: Block.Timestamp,
                       generationSignature: GenerationSignature,
                       baseTarget: BaseTarget,
                       generator: PublicKey25519Proposition,
                       txs: Seq[BifrostTransaction])
  extends Block[PublicKey25519Proposition, BifrostTransaction] {

  override type M = BifrostBlock

  override lazy val modifierTypeId: Byte = BifrostBlock.ModifierTypeId

  override lazy val transactions: Option[Seq[BifrostTransaction]] = Some(txs)

  override lazy val serializer = BifrostBlockCompanion

  override lazy val id: BlockId = FastCryptographicHash(serializer.messageToSign(this))

  override lazy val version: Version = 0: Byte

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generationSignature" -> Base58.encode(generationSignature).asJson,
    "baseTarget" -> baseTarget.asJson,
    "generator" -> Base58.encode(generator.pubKeyBytes).asJson,
    "txs" -> txs.map(_.json).asJson
  ).asJson
}

object BifrostBlock {
  val ModifierTypeId = 3: Byte

  val SignatureLength = 64

  type GenerationSignature = Array[Byte]

  type BaseTarget = Long
}

object BifrostBlockCompanion extends Serializer[BifrostBlock] {

  def messageToSign(block: BifrostBlock): Array[Byte] = {
    block.parentId ++
      Longs.toByteArray(block.timestamp) ++
      Array(block.version) ++
      Longs.toByteArray(block.baseTarget) ++
      block.generator.pubKeyBytes ++ {
      val cntBytes = Ints.toByteArray(block.txs.length)

      // writes number of transactions, then adds <tx as bytes>| <number of bytes for tx as bytes> for each tx
      block.txs.foldLeft(cntBytes) { case (bytes, tx) => bytes ++ tx.bytes ++ Ints.toByteArray(tx.bytes.length) }
    }
  }

  override def toBytes(block: BifrostBlock): Array[Byte] = {
    messageToSign(block)
  }

  override def parseBytes(bytes: Array[ModifierTypeId]): Try[BifrostBlock] = Try {
    val parentId = bytes.slice(0, Block.BlockIdLength)
    val timestamp = Longs.fromByteArray(bytes.slice(Block.BlockIdLength, Block.BlockIdLength + 8))
    val version = bytes.slice(Block.BlockIdLength + 8, Block.BlockIdLength + 9).head
    val s0 = Block.BlockIdLength + 9
    val generationSignature = bytes.slice(s0, s0 + BifrostBlock.SignatureLength)
    val baseTarget = Longs.fromByteArray(bytes.slice(s0 + BifrostBlock.SignatureLength, s0 + BifrostBlock.SignatureLength + 8))
    val s1 = s0 + BifrostBlock.SignatureLength + 8
    val generator = PublicKey25519Proposition(bytes.slice(s1, s1 + 32))
    val cnt = Ints.fromByteArray(bytes.slice(s1 + 32, s1 + 36))
    val s2 = s1 + 36

    def unfoldRight[A,B](seed: B)(f: B => Option[(A, B)]): Seq[A] = {
      f(seed) match {
        case Some((a, b)) => unfoldRight(b)(f) :+ a
        case None => Nil
      }
    }
    val txBytes = bytes.slice(s2, bytes.length)

    val tx: Seq[BifrostTransaction] = unfoldRight(txBytes)(bytes => {
      val bytesToGrab = Ints.fromByteArray(bytes.slice(bytes.length-4, bytes.length))

      if(bytes.length < bytesToGrab + 4)
        None
      else {
        val thisTx = bytes.slice(bytes.length - bytesToGrab - 4, bytes.length - 4)
        Some((thisTx, bytes.slice(0, bytes.length - bytesToGrab - 4)))
      }
    }).ensuring(_.length == cnt).map(tx => BifrostTransactionCompanion.parseBytes(tx).get)

    BifrostBlock(parentId, timestamp, generationSignature, baseTarget, generator, tx)
  }
}