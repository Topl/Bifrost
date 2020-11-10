package co.topl

import java.net.{ InetAddress, InetSocketAddress }

import co.topl.attestation.proposition.PublicKeyPropositionCurve25519
import co.topl.attestation.secrets.PrivateKeyCurve25519
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.{ ModifierId, NodeViewModifier }
import co.topl.network.message._
import co.topl.settings.Version
import org.scalacheck.{ Arbitrary, Gen }
import scorex.crypto.signatures.Curve25519

//Generators of objects from scorex-core
trait CoreGenerators {

  lazy val smallInt: Gen[Int] = Gen.choose(0, 20)

  lazy val nonEmptyBytesGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(_.toArray).suchThat(_.length > 0)
  lazy val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)
  lazy val positiveByteGen: Gen[Byte] = Gen.choose(1, Byte.MaxValue)
  lazy val modifierIdGen: Gen[ModifierId] =
    Gen.listOfN(NodeViewModifier.ModifierIdSize, Arbitrary.arbitrary[Byte]).map(li => ModifierId(li.toArray))
  lazy val invDataGen: Gen[InvData] = for {
    modifierTypeIdByte: Byte <- Arbitrary.arbitrary[Byte]
    modifierTypeId: ModifierTypeId = ModifierTypeId @@ modifierTypeIdByte
    modifierIds: Seq[ModifierId] <- Gen.nonEmptyListOf(modifierIdGen) if modifierIds.nonEmpty
  } yield InvData(modifierTypeId, modifierIds)
  lazy val modifierWithIdGen: Gen[(ModifierId, Array[Byte])] = for {
    id <- modifierIdGen
    mod <- nonEmptyBytesGen
  } yield id -> mod
  lazy val modifiersGen: Gen[ModifiersData] = for {
    modifierTypeIdByte: Byte <- Arbitrary.arbitrary[Byte]
    modifierTypeId: ModifierTypeId = ModifierTypeId @@ modifierTypeIdByte
    modifiers: Map[ModifierId, Array[Byte]] <- Gen.nonEmptyMap(modifierWithIdGen).suchThat(_.nonEmpty)
  } yield ModifiersData(modifierTypeId, modifiers)
  lazy val appVersionGen: Gen[Version] = for {
    fd <- Gen.choose(0: Byte, MaxVersion)
    sd <- Gen.choose(0: Byte, MaxVersion)
    td <- Gen.choose(0: Byte, MaxVersion)
  } yield new Version(fd, sd, td)
  lazy val inetSocketAddressGen = for {
    ip1 <- Gen.choose(0, MaxIp)
    ip2 <- Gen.choose(0, MaxIp)
    ip3 <- Gen.choose(0, MaxIp)
    ip4 <- Gen.choose(0, MaxIp)
    port <- Gen.choose(0, MaxPort)
  } yield new InetSocketAddress(InetAddress.getByName(s"$ip1.$ip2.$ip3.$ip4"), port)
  lazy val key25519Gen: Gen[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)] = genBytesList(Curve25519.KeyLength)
    .map(s => PrivateKeyCurve25519.generateKeys(s))
  lazy val propositionGen: Gen[PublicKeyPropositionCurve25519] = key25519Gen.map(_._2)
  val MaxVersion: Byte = 999.toByte
  val MaxIp = 255
  val MaxPort = 65535

  def genBytesList(size: Int): Gen[Array[Byte]] = genBoundedBytes(size, size)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }
}
