package co.topl.codecs.json.modifier.block

import cats.implicits._
import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.crypto.hash.digest.Digest32
import co.topl.modifier.ModifierId
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.block.{Block, BlockBody, BlockHeader, BloomFilter}
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.transaction.Transaction
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.StringDataTypes.implicits._
import co.topl.utils.TimeProvider
import co.topl.codecs.binary._
import co.topl.codecs.json.crypto._
import co.topl.codecs.json.modifier.box._
import co.topl.codecs.json.valuetypes._
import io.circe.syntax._
import io.circe._
import co.topl.codecs.json.modifier.transaction._
import co.topl.modifier.block.BloomFilter.fromBase58

trait BlockCodecs {

  implicit val modifierIdJsonEncoder: Encoder[ModifierId] = _.transmittableBase58.asJson

  implicit val modifierIdJsonKeyEncoder: KeyEncoder[ModifierId] = _.transmittableBase58.show

  implicit val modifierIdJsonDecoder: Decoder[ModifierId] =
    Decoder[Base58Data].emap(_.decodeTransmitted[ModifierId])

  implicit val modifierIdJsonKeyDecoder: KeyDecoder[ModifierId] =
    KeyDecoder[Base58Data].map(_.decodeTransmitted[ModifierId].getOrThrow())

  implicit val blockJsonEncoder: Encoder[Block] = { b: Block =>
    val (header, body) = b.toComponents
    Map(
      "header"    -> header.asJson,
      "body"      -> body.asJson,
      "blockSize" -> b.persistedBytes.length.asJson
    ).asJson
  }

  implicit def blockJsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[Block] = (c: HCursor) =>
    for {
      header <- c.downField("header").as[BlockHeader]
      body   <- c.downField("body").as[BlockBody]
    } yield Block.fromComponents(header, body)

  implicit val blockBodyJsonEncoder: Encoder[BlockBody] = { b: BlockBody =>
    Map(
      "id"       -> b.id.asJson,
      "parentId" -> b.parentId.asJson,
      "txs"      -> b.transactions.asJson,
      "version"  -> b.version.asJson
    ).asJson
  }

  implicit def blockBodyJsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[BlockBody] = (c: HCursor) =>
    for {
      id       <- c.downField("id").as[ModifierId]
      parentId <- c.downField("parentId").as[ModifierId]
      txsSeq   <- c.downField("txs").as[Seq[Transaction.TX]]
      version  <- c.downField("version").as[PNVMVersion]
    } yield BlockBody(id, parentId, txsSeq, version)

  implicit val blockHeaderJsonEncoder: Encoder[BlockHeader] = { bh: BlockHeader =>
    Map(
      "id"           -> bh.id.asJson,
      "parentId"     -> bh.parentId.asJson,
      "timestamp"    -> bh.timestamp.asJson,
      "generatorBox" -> bh.generatorBox.asJson,
      "publicKey"    -> bh.publicKey.asJson,
      "signature"    -> bh.signature.asJson,
      "height"       -> bh.height.asJson,
      "difficulty"   -> bh.difficulty.asJson,
      "txRoot"       -> bh.txRoot.asJson,
      "bloomFilter"  -> bh.bloomFilter.asJson,
      "version"      -> bh.version.asJson
    ).asJson
  }

  implicit val blockHeaderDecoder: Decoder[BlockHeader] = (c: HCursor) =>
    for {
      id           <- c.downField("id").as[ModifierId]
      parentId     <- c.downField("parentId").as[ModifierId]
      timestamp    <- c.downField("timestamp").as[TimeProvider.Time]
      generatorBox <- c.downField("generatorBox").as[ArbitBox]
      publicKey    <- c.downField("publicKey").as[PublicKeyPropositionCurve25519]
      signature    <- c.downField("signature").as[SignatureCurve25519]
      height       <- c.downField("height").as[Long]
      difficulty   <- c.downField("difficulty").as[Long]
      txRoot       <- c.downField("txRoot").as[Digest32]
      bloomFilter  <- c.downField("bloomFilter").as[BloomFilter]
      version      <- c.downField("version").as[Byte]
    } yield BlockHeader(
      id,
      parentId,
      timestamp,
      generatorBox,
      publicKey,
      signature,
      height,
      difficulty,
      txRoot,
      bloomFilter,
      version
    )

  implicit val bloomFilterJsonEncoder: Encoder[BloomFilter] = (bf: BloomFilter) => bf.toString.asJson

  implicit val bloomFilterJsonKeyEncoder: KeyEncoder[BloomFilter] = (bf: BloomFilter) => bf.toString

  implicit val bloomFilterJsonDecoder: Decoder[BloomFilter] = Decoder[Base58Data].map(BloomFilter.fromBase58)
}
