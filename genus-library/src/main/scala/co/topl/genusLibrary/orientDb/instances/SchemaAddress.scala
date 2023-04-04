package co.topl.genusLibrary.orientDb.instances

import co.topl.brambl.models.{Address, Identifier}
import co.topl.genusLibrary.orientDb.schema.OIndexable.Instances._
import co.topl.genusLibrary.orientDb.schema.OTyped.Instances._
import co.topl.genusLibrary.orientDb.schema.{GraphDataEncoder, VertexSchema}

object SchemaAddress {

  /**
   * Address model:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/brambl/models/address.proto
   */
  object Field {
    val SchemaName = "Address"
    val Network = "network"
    val Ledger = "ledger"
    val Index = "index"
    // id on proto models, do not use id, Property key is reserved for all elements: id
    val AddressId = "addressId"
    val AddressIndex = "addressIndex"
  }

  def make(): VertexSchema[Address] =
    VertexSchema.create(
      Field.SchemaName,
      GraphDataEncoder[Address]
        .withProperty(
          Field.Network,
          address => java.lang.Integer.valueOf(address.network),
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.Ledger,
          address => java.lang.Integer.valueOf(address.ledger),
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.Index,
          address => java.lang.Integer.valueOf(address.index),
          mandatory = false,
          readOnly = true,
          notNull = false
        )
        .withProperty(
          Field.AddressId,
          _.id.toByteArray,
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withIndex[Address](Field.AddressIndex, Field.AddressId),
      v =>
        Address(
          network = v(Field.Network),
          ledger = v(Field.Ledger),
          index = v(Field.Index),
          id = Identifier.parseFrom(v(Field.AddressId): Array[Byte])
        )
    )

}
