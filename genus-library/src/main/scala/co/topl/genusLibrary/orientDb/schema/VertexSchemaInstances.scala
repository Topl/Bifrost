package co.topl.genusLibrary.orientDb.schema

import co.topl.brambl.models.box.Box
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.{Evidence, Identifier, LockAddress, TransactionOutputAddress}
import co.topl.codecs.bytes.tetra.instances.{blockHeaderAsBlockHeaderOps, ioTransactionAsIoTransactionOps}
import co.topl.consensus.models.{BlockHeader, BlockId, EligibilityCertificate, OperationalCertificate}
import co.topl.genus.services.{Txo, TxoState}
import co.topl.genusLibrary.orientDb.schema.OrientDbTyped.Instances._
import co.topl.node.models.BlockBody
import com.google.protobuf.ByteString
import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
import quivr.models.Digest

/**
 * Metadata describing the schema used for the Genus graph in OrientDB
 */
object VertexSchemaInstances {

  trait Instances {

    /**
     * Schema for Address nodes
     */
    implicit private[genusLibrary] val addressVertexSchema: VertexSchema[LockAddress] =
      VertexSchema.create(
        "LockAddress",
        GraphDataEncoder[LockAddress]
          .withProperty("network", v => java.lang.Integer.valueOf(v.network), _.setNotNull(true))
          .withProperty("ledger", v => java.lang.Integer.valueOf(v.ledger), _.setNotNull(true))
          .withProperty(
            "id",
            _.id match {
              case v: LockAddress.Id.Lock32 => Array[Byte](0) ++ v.value.toByteArray
              case v: LockAddress.Id.Lock64 => Array[Byte](1) ++ v.value.toByteArray
            },
            _.setNotNull(true)
          ),
        v => {
          val idData: Array[Byte] = v("id")
          val id = idData(0) match {
            case 0 => LockAddress.Id.Lock32(Identifier.Lock32.parseFrom(idData.tail))
            case 1 => LockAddress.Id.Lock64(Identifier.Lock64.parseFrom(idData.tail))
          }
          LockAddress(v("network"), v("ledger"), id)
        }
      )

    /**
     * Schema for TxO state vertexes
     * <p>
     * address state vertexes have no properties, just links to txoStates.
     */
    implicit private[genusLibrary] val addressStateSchema: VertexSchema[Unit] =
      VertexSchema.create(
        "AddressState",
        GraphDataEncoder[Unit],
        _ => ()
      )

    implicit private[genusLibrary] val blockHeaderSchema: VertexSchema[BlockHeader] =
      VertexSchema.create(
        "BlockHeader",
        GraphDataEncoder[BlockHeader]
          .withProperty(
            "blockId",
            _.id.value.toByteArray,
            _.setNotNull(true).setReadonly(true).setMandatory(true)
          )(byteArrayOrientDbTypes)
          .withProperty(
            "parentHeaderId",
            _.parentHeaderId.value.toByteArray,
            _.setNotNull(true).setReadonly(true).setMandatory(true)
          )(byteArrayOrientDbTypes)
          .withProperty(
            "parentSlot",
            l => java.lang.Long.valueOf(l.parentSlot),
            _.setReadonly(true)
          )(longOrientDbTyped)
          .withProperty(
            "txRoot",
            _.txRoot.toByteArray,
            _.setReadonly(true)
          )(byteArrayOrientDbTypes)
          .withProperty(
            "bloomFilter",
            _.bloomFilter.toByteArray,
            _.setReadonly(true)
          )(byteArrayOrientDbTypes)
          .withProperty(
            "timestamp",
            ts => java.lang.Long.valueOf(ts.timestamp),
            _.setNotNull(true).setReadonly(true).setMandatory(true)
          )(longOrientDbTyped)
          .withProperty(
            "height",
            ht => java.lang.Long.valueOf(ht.height),
            _.setNotNull(true).setReadonly(true).setMandatory(true)
          )(longOrientDbTyped)
          .withProperty(
            "slot",
            s => java.lang.Long.valueOf(s.slot),
            _.setNotNull(true).setReadonly(true).setMandatory(true)
          )(longOrientDbTyped)
          .withProperty(
            "eligibilityCertificate",
            e => e.eligibilityCertificate.toByteArray,
            _.setNotNull(true).setReadonly(true).setMandatory(true)
          )(
            byteArrayOrientDbTypes
          )
          .withProperty(
            "operationalCertificate",
            _.operationalCertificate.toByteArray,
            _.setNotNull(true).setReadonly(true).setMandatory(true)
          )(
            byteArrayOrientDbTypes
          )
          .withProperty(
            "metadata",
            _.metadata.toByteArray,
            _.setNotNull(false).setReadonly(true).setMandatory(true)
          )(byteArrayOrientDbTypes)
          .withProperty(
            "StakingAddress",
            _.address.toByteArray,
            _.setNotNull(true).setReadonly(true).setMandatory(true)
          )(
            byteArrayOrientDbTypes
          )
          .withIndex("blockHeaderIndex", INDEX_TYPE.UNIQUE, "blockId"),
        v =>
          BlockHeader(
            BlockId(ByteString.copyFrom(v("parentHeaderId"): Array[Byte])),
            v("parentSlot"),
            v("txRoot"),
            v("bloomFilter"),
            v("timestamp"),
            v("height"),
            v("slot"),
            EligibilityCertificate.parseFrom((v("eligibilityCertificate"))),
            OperationalCertificate.parseFrom(v("operationalCertificate")),
            v("metadata"),
            v("StakingAddress")
          )
      )

    implicit private[genusLibrary] val blockBodySchema: VertexSchema[BlockBody] =
      VertexSchema.create(
        "BlockBody",
        GraphDataEncoder[BlockBody]
          .withProperty("transactionIds", blockBody => blockBody.toByteArray, _ => {})(byteArrayOrientDbTypes),
        // There is no index needed for block bodies. They are accessed thru links from block headers and transactions
        v => BlockBody.parseFrom(v("transactionIds"))
      )

    implicit private[genusLibrary] val transactionSchema: VertexSchema[IoTransaction] =
      VertexSchema.create(
        "Transaction",
        GraphDataEncoder[IoTransaction]
          .withProperty(
            "transactionId",
            t => t.id.toByteArray,
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
          .withProperty("transaction", _.toByteArray, _.setNotNull(true))(byteArrayOrientDbTypes)
          .withIndex("transactionIdIndex", INDEX_TYPE.UNIQUE, "transactionId"),
        // transactionID is not stored in a transaction, but computed
        v => IoTransaction.parseFrom(v("transaction"))
      )

    implicit private[genusLibrary] val txoSchema: VertexSchema[Txo] =
      VertexSchema.create(
        "TxoState",
        GraphDataEncoder[Txo]
          .withProperty(
            "transactionId",
            _.outputAddress.get.getIoTransaction32.evidence.digest.value.toByteArray,
            _.setNotNull(true)
          )(
            byteArrayOrientDbTypes
          )
          .withProperty(
            "transactionOutputIndex",
            txo => java.lang.Short.valueOf(txo.outputAddress.get.index.toShort),
            _.setNotNull(true)
          )(shortOrientDbTyped)
          // TODO, see the index below
//          .withProperty("assetLabel", _.assetLabel, _.setNotNull(true))(stringOrientDbTyped)
          .withProperty("box", txo => txo.box.toByteArray, _ => ())(byteArrayOrientDbTypes)
          .withProperty("state", _.state.toString, _ => ())(stringOrientDbTyped)
          .withProperty(
            "address",
            _.lockAddress.map(_.getLock32.evidence.digest.value.toByteArray).orNull,
            _.setNotNull(false)
          )(byteArrayOrientDbTypes)
          .withIndex("boxId", INDEX_TYPE.UNIQUE, "transactionId", "transactionOutputIndex")
        // TODO assetLabel was disabled on https://github.com/Topl/Bifrost/pull/2850
        // .withIndex("assetLabel", INDEX_TYPE.NOTUNIQUE, "assetLabel")
        ,
        v => {
          val transactionIdBytes: Array[Byte] = v("transactionId")
          val transactionId = TransactionOutputAddress.Id.IoTransaction32(
            Identifier.IoTransaction32(Evidence.Sized32(Digest.Digest32(ByteString.copyFrom(transactionIdBytes))))
          )
          val txoAddress = TransactionOutputAddress(0, 0, v("transactionOutputIndex"), transactionId)
          Txo(
            Box.parseFrom(v("box")),
            TxoState.values.find(_.name == v("state")).get,
            Some(txoAddress),
            None // TODO
          )
        }
      )
  }
  object instances extends Instances
}
