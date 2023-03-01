package co.topl.codecs.bytes

/**
 * This package contains the codecs used to serialize/deserialize bifrost object to/from BitVector. BitVectors are
 * easily converted to/from Array[Byte]
 */
package object tetra {

  object instances
      extends TetraBinaryShowCodecs
      with TetraIdentifiableInstances
      with ProtoIdentifiableOps
      with TetraPersistableCodecs
//      with TetraScodecCodecs
      with TetraSignableCodecs
      with TetraImmutableCodecs
      with TetraTransmittableCodecs
}
