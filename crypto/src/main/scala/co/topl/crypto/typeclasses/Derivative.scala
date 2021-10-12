//package co.topl.crypto.typeclasses
//
//import co.topl.codecs.bytes.BasicCodecs._
//import co.topl.codecs.bytes.ByteCodec.implicits._
//import co.topl.crypto.signing.Ed25519
//import co.topl.crypto.typeclasses.ContainsVerificationKey.instances._
//import co.topl.models.utility.HasLength.instances._
//import co.topl.models.utility.Lengths._
//import co.topl.models.utility.{Lengths, Sized}
//import co.topl.models.{Bytes, SecretKeys, VerificationKeys}
//import org.bouncycastle.crypto.digests.SHA512Digest
//import org.bouncycastle.crypto.macs.HMac
//import org.bouncycastle.crypto.params.KeyParameter
//import simulacrum.{op, typeclass}
//
//import java.nio.{ByteBuffer, ByteOrder}
//
///**
// * Represents a value that can be further derived using some index
// */
//@typeclass trait Derivative[T] {
//
//  /**
//   * Derives a new value T at some index using a previous value of T
//   */
//  @op("Derive") def softDerivativeOf(t: T, index: Derivative.KeyIndexes.Soft): T
//}
