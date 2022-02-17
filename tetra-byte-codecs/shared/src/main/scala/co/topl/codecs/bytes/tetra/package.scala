package co.topl.codecs.bytes

package object tetra {

  object instances
      extends TetraBinaryShowCodecs
      with TetraIdentifiableInstances
      with TetraPersistableCodecs
//      with TetraScodecCodecs
      with TetraSignableCodecs
      with TetraImmutableCodecs
      with TetraTransmittableCodecs
}
