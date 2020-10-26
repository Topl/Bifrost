package co.topl

import co.topl.crypto.PrivateKey25519
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.signatures.Curve25519

trait CoreGenerators {

  lazy val nonEmptyBytesGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(_.toArray).suchThat(_.length > 0)
  lazy val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)
  lazy val modifierIdGen: Gen[ModifierId] =
    Gen.listOfN(NodeViewModifier.ModifierIdSize, Arbitrary.arbitrary[Byte]).map(li => ModifierId(li.toArray))
  lazy val key25519Gen: Gen[(PrivateKey25519, PublicKey25519Proposition)] = genBytesList(Curve25519.KeyLength)
    .map(s => PrivateKey25519.generateKeys(s))
  lazy val propositionGen: Gen[PublicKey25519Proposition] = key25519Gen.map(_._2)

  def genBytesList(size: Int): Gen[Array[Byte]] = genBoundedBytes(size, size)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }
}
