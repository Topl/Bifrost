package co.topl.genusLibrary.orientDb.instances

import co.topl.brambl.models.{LockAddress, LockId}
import co.topl.genusLibrary.orientDb.schema.OIndexable.Instances._
import co.topl.genusLibrary.orientDb.schema.OTyped.Instances._
import co.topl.genusLibrary.orientDb.schema.{GraphDataEncoder, VertexSchema}
import scodec.bits.BitVector

object SchemaLockAddress {

  /**
   * Address model:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/brambl/models/address.proto
   */
  object Field {
    val SchemaName = "LockAddress"
    val Network = "network"
    val Ledger = "ledger"
    // id on proto models, do not use id, Property key is reserved for all elements: id
    val AddressId = "addressId"
    val AddressEncodedId = "addressEncodedId"
    val AddressIndex = "addressIndex"
  }

  def make(): VertexSchema[LockAddress] =
    VertexSchema.create(
      Field.SchemaName,
      GraphDataEncoder[LockAddress]
        .withProperty(
          Field.Network,
          lockAddress => java.lang.Integer.valueOf(lockAddress.network),
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.Ledger,
          lockAddress => java.lang.Integer.valueOf(lockAddress.ledger),
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.AddressId,
          _.id.toByteArray,
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.AddressEncodedId,
          // TODO it should be updated to use the new address encoding scheme.
          lockAddress => BitVector(lockAddress.id.toByteArray).toBase58,
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withIndex[LockAddress](Field.AddressIndex, Field.AddressId),
      v =>
        LockAddress(
          network = v(Field.Network),
          ledger = v(Field.Ledger),
          id = LockId.parseFrom(v(Field.AddressId))
        )
    )
}
