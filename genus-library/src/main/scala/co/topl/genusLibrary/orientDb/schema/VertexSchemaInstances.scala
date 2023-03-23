package co.topl.genusLibrary.orientDb.schema

import co.topl.brambl.models.box.Box
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.{Evidence, Identifier, LockAddress, TransactionOutputAddress}
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services.{Txo, TxoState}
import co.topl.genusLibrary.orientDb.schema.OrientDbTyped.Instances._
import co.topl.node.models.BlockBody
import com.google.protobuf.ByteString
import quivr.models.Digest

/**
 * Metadata describing the schema used for the Genus graph in OrientDB
 */
object VertexSchemaInstances {

  trait Instances {

    private[genusLibrary] val blockHeaderSchema: VertexSchema[BlockHeader] = SchemaBlockHeader.make()
    private[genusLibrary] val blockBodySchema: VertexSchema[BlockBody] = SchemaBlockBody.make()
    private[genusLibrary] val ioTransactionSchema: VertexSchema[IoTransaction] = SchemaIoTransaction.make()

    // Note, From here to the end, VertexSchemas not tested
    /**
     * Schema for Address nodes
     */
    implicit private[genusLibrary] val addressVertexSchema: VertexSchema[LockAddress] =
      VertexSchema.create(
        "LockAddress",
        GraphDataEncoder[LockAddress]
          .withProperty(
            "network",
            v => java.lang.Integer.valueOf(v.network),
            mandatory = false,
            readOnly = false,
            notNull = true
          )
          .withProperty(
            "ledger",
            v => java.lang.Integer.valueOf(v.ledger),
            mandatory = false,
            readOnly = false,
            notNull = true
          )
          .withProperty(
            "id",
            _.id match {
              case v: LockAddress.Id.Lock32 => Array[Byte](0) ++ v.value.toByteArray
              case v: LockAddress.Id.Lock64 => Array[Byte](1) ++ v.value.toByteArray
            },
            mandatory = false,
            readOnly = false,
            notNull = true
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

    implicit private[genusLibrary] val txoSchema: VertexSchema[Txo] =
      VertexSchema.create(
        "TxoState",
        GraphDataEncoder[Txo]
          .withProperty(
            "transactionId",
            _.outputAddress.get.getIoTransaction32.evidence.digest.value.toByteArray,
            mandatory = false,
            readOnly = false,
            notNull = true
          )(
            byteArrayOrientDbTypes
          )
          .withProperty(
            "transactionOutputIndex",
            txo => java.lang.Short.valueOf(txo.outputAddress.get.index.toShort),
            mandatory = false,
            readOnly = false,
            notNull = true
          )(shortOrientDbTyped)
          // TODO, see the index below
//          .withProperty("assetLabel", _.assetLabel, _.setNotNull(true))(stringOrientDbTyped)
          .withProperty("box", txo => txo.box.toByteArray, mandatory = false, readOnly = false, notNull = false)(
            byteArrayOrientDbTypes
          )
          .withProperty("state", _.state.toString, mandatory = false, readOnly = false, notNull = false)(
            stringOrientDbTyped
          )
          .withProperty(
            "address",
            _.lockAddress.map(_.getLock32.evidence.digest.value.toByteArray).orNull,
            mandatory = false,
            readOnly = false,
            notNull = false
          )(byteArrayOrientDbTypes)
//          .withIndex("boxId", INDEX_TYPE.UNIQUE, "transactionId", "transactionOutputIndex") // TODO create index type class instance
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
