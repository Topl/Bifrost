package co.topl.typeclasses.crypto

import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.kes.keys.SymmetricKey
import co.topl.crypto.signatures.{Ed25519, Ed25519VRF}
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import simulacrum.typeclass

import java.security.SecureRandom

@typeclass trait KeyInitializer[SK <: PrivateKey] {
  self =>
  def random(): SK
  def fromSeed(seed: Bytes): SK
}

object KeyInitializer {

  trait Instances {

    implicit val ed25519Initializer: KeyInitializer[PrivateKeys.Ed25519] =
      new KeyInitializer[PrivateKeys.Ed25519] {
        private val instance = new Ed25519

        override def random(): PrivateKeys.Ed25519 =
          (fromInstanceTypes _).tupled(instance.createKeyPair)

        override def fromSeed(seed: Bytes): PrivateKeys.Ed25519 =
          (fromInstanceTypes _).tupled(instance.createKeyPair(seed.toArray))

        private def fromInstanceTypes(priv: co.topl.crypto.PrivateKey, pub: co.topl.crypto.PublicKey) =
          PrivateKeys.Ed25519(
            Sized.strictUnsafe[Bytes, PrivateKeys.Ed25519.Length](Bytes(priv.value))
          )
      }

    implicit val vrfInitializer: KeyInitializer[PrivateKeys.Vrf] =
      new KeyInitializer[PrivateKeys.Vrf] {

        def random(): PrivateKeys.Vrf =
          (fromLib _).tupled(Ed25519VRF.instance.createKeyPair)

        def fromSeed(seed: Bytes): PrivateKeys.Vrf =
          (fromLib _).tupled(Ed25519VRF.instance.createKeyPair(seed.toArray))

        private def fromLib(sk: co.topl.crypto.PrivateKey, pk: co.topl.crypto.PublicKey): PrivateKeys.Vrf =
          PrivateKeys.Vrf(PrivateKeys.Ed25519(Sized.strictUnsafe(Bytes(sk.value))))
      }

    implicit def kesInitializer(implicit slot: Slot): KeyInitializer[PrivateKeys.Kes] =
      new KeyInitializer[PrivateKeys.Kes] {
        private val scheme = new KeyEvolvingSignatureScheme

        def random(): PrivateKeys.Kes =
          fromLib(scheme.generateSymmetricProductKey(new SecureRandom().generateSeed(32), offset = slot))

        def fromSeed(seed: Bytes): PrivateKeys.Kes =
          fromLib(scheme.generateSymmetricProductKey(seed.toArray, offset = slot))

        private def fromLib(key: SymmetricKey): PrivateKeys.Kes =
          PrivateKeys.Kes(Sized.strictUnsafe(Bytes(key.getBytes)))

      }
  }
  object Instances extends Instances
}
