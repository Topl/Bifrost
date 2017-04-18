package examples.bifrost.blocks

import com.google.common.primitives.{Bytes, Ints, Longs}
import examples.bifrost.transaction.{BifrostTransaction, BifrostTransactionCompanion}
import examples.curvepos.transaction.SimpleBlock._
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.{Constants25519, ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.Try

case class BifrostBlock(override val parentId: BlockId,
                       override val timestamp: Block.Timestamp,
                       generationSignature: GenerationSignature,
                       baseTarget: BaseTarget,
                       generator: PublicKey25519Proposition,
                       txs: Seq[BifrostTransaction])
  extends Block[ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction] {

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

    val numTx = Ints.toByteArray(block.txs.length)

    Bytes.concat(
        block.parentId,
        Longs.toByteArray(block.timestamp),
        Array(block.version),
        block.generationSignature,
        Longs.toByteArray(block.baseTarget),
        block.generator.pubKeyBytes,
        numTx,  // writes number of transactions, then adds <tx as bytes>| <number of bytes for tx as bytes> for each tx
        block.txs.foldLeft(Array[Byte]())((bytes, tx) => bytes ++ Ints.toByteArray(BifrostTransactionCompanion.toBytes(tx).length) ++ BifrostTransactionCompanion.toBytes(tx))
    )
  }

  override def toBytes(block: BifrostBlock): Array[Byte] = {
    messageToSign(block)
  }

  override def parseBytes(bytes: Array[ModifierTypeId]): Try[BifrostBlock] = Try {

    val parentId = bytes.slice(0, Block.BlockIdLength)

    val timestamp = Longs.fromByteArray(bytes.slice(Block.BlockIdLength, Block.BlockIdLength + Longs.BYTES))

    val version = bytes.slice(Block.BlockIdLength + Longs.BYTES, Block.BlockIdLength + Longs.BYTES + 1).head

    var numBytesRead = Block.BlockIdLength + Longs.BYTES + 1

    val generationSignature = bytes.slice(numBytesRead, numBytesRead + BifrostBlock.SignatureLength)
    val baseTarget = Longs.fromByteArray(bytes.slice(numBytesRead + BifrostBlock.SignatureLength, numBytesRead + BifrostBlock.SignatureLength + Longs.BYTES))

    numBytesRead += BifrostBlock.SignatureLength + Longs.BYTES

    val generator = PublicKey25519Proposition(bytes.slice(numBytesRead, numBytesRead + Constants25519.PubKeyLength))
    val numTxExpected = Ints.fromByteArray(bytes.slice(numBytesRead + Constants25519.PubKeyLength, numBytesRead + Constants25519.PubKeyLength + Ints.BYTES))
    numBytesRead += Constants25519.PubKeyLength + Ints.BYTES

    def unfoldLeft[A,B](seed: B)(f: B => Option[(A, B)]): Seq[A] = {
      f(seed) match {
        case Some((a, b)) => a +: unfoldLeft(b)(f)
        case None => Nil
      }
    }

    val txBytes: Array[Byte] = bytes.slice(numBytesRead, bytes.length)

    val txByteSeq: Seq[Array[Byte]] = unfoldLeft(txBytes)(bytes => bytes match {
      case b if b.length < Ints.BYTES => None
      case _ =>
        val bytesToGrab = Ints.fromByteArray(bytes.take(Ints.BYTES))

        if(bytes.length - Ints.BYTES < bytesToGrab) {
          None // we're done because we can't grab the number of bytes required
        } else {
          val thisTx: Array[Byte] = bytes.slice(Ints.BYTES, Ints.BYTES + bytesToGrab)
          Some((thisTx, bytes.slice(Ints.BYTES + bytesToGrab, bytes.length)))
        }
    }).ensuring(_.length == numTxExpected)

    val tx: Seq[BifrostTransaction] = txByteSeq.map(tx => BifrostTransactionCompanion.parseBytes(tx).get)

    BifrostBlock(parentId, timestamp, generationSignature, baseTarget, generator, tx)
  }
}