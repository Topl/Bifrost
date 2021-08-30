package co.topl.typeclasses.crypto

import cats.Functor
import cats.implicits._
import co.topl.crypto.signatures.Ed25519
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

  object Instances {

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
      ed25519Initializer.map(edKeyPair =>
        KeyPairs.Vrf(PrivateKeys.Vrf(edKeyPair.privateKey), PublicKeys.Vrf(edKeyPair.publicKey))
      )
  }
}
