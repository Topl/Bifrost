package co.topl.typeclasses.crypto

import cats.Functor
import co.topl.crypto.signatures.{Ed25519, Ed25519VRF}
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import simulacrum.typeclass

@typeclass trait KeyInitializer[KP] {
  self =>
  def random(): KP
  def fromSeed(seed: Bytes): KP
}

object KeyInitializer {

  trait Instances {

    implicit val functor: Functor[KeyInitializer] =
      new Functor[KeyInitializer] {

        override def map[A, B](fa: KeyInitializer[A])(f: A => B): KeyInitializer[B] =
          new KeyInitializer[B] {
            override def random(): B = f(fa.random())

            override def fromSeed(seed: Bytes): B = f(fa.fromSeed(seed))
          }

      }

    implicit val ed25519Initializer: KeyInitializer[KeyPairs.Ed25519] =
      new KeyInitializer[KeyPairs.Ed25519] {
        private val instance = new Ed25519

        override def random(): KeyPairs.Ed25519 =
          (fromInstanceTypes _).tupled(instance.createKeyPair)

        override def fromSeed(seed: Bytes): KeyPairs.Ed25519 =
          (fromInstanceTypes _).tupled(instance.createKeyPair(seed.toArray))

        private def fromInstanceTypes(priv: co.topl.crypto.PrivateKey, pub: co.topl.crypto.PublicKey) =
          KeyPairs.Ed25519(
            PrivateKeys.Ed25519(
              Sized.strict[Bytes, PrivateKeys.Ed25519.Length](Bytes(priv.value)).toOption.get
            ),
            PublicKeys.Ed25519(
              Sized.strict[Bytes, PublicKeys.Ed25519.Length](Bytes(priv.value)).toOption.get
            )
          )
      }

    implicit val vrfInitializer: KeyInitializer[KeyPairs.Vrf] =
      new KeyInitializer[KeyPairs.Vrf] {

        def random(): KeyPairs.Vrf =
          (fromLib _).tupled(Ed25519VRF.instance.createKeyPair)

        def fromSeed(seed: Bytes): KeyPairs.Vrf =
          (fromLib _).tupled(Ed25519VRF.instance.createKeyPair(seed.toArray))

        private def fromLib(sk: co.topl.crypto.PrivateKey, pk: co.topl.crypto.PublicKey): KeyPairs.Vrf = {
          for {
            edSkBytes <- Sized.strict[Bytes, PrivateKeys.Ed25519.Length](Bytes(sk.value))
            edPkBytes <- Sized.strict[Bytes, PublicKeys.Ed25519.Length](Bytes(pk.value))
          } yield KeyPairs.Vrf(
            PrivateKeys.Vrf(PrivateKeys.Ed25519(edSkBytes)),
            PublicKeys.Vrf(PublicKeys.Ed25519(edPkBytes))
          )
        }.toOption.get

      }
  }
  object Instances extends Instances
}
