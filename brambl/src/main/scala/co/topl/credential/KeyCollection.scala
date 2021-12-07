package co.topl.credential

import cats._
import cats.data.OptionT
import cats.effect._
import cats.implicits._
import co.topl.codecs.bytes.ByteCodec
import co.topl.codecs.bytes.implicits._
import co.topl.crypto.signing.Password
import co.topl.models._

trait KeyCollection[F[_]] {
  def unlockedEntries: F[Set[TypedEvidence]]
  def unlock[SK <: SecretKey: ByteCodec](evidence: TypedEvidence, password: Password): F[Option[SK]]
  def lift[SK <: SecretKey](evidence:              TypedEvidence): F[Option[SK]]
}

class RefKeyCollection[F[_]: Monad](r: Ref[F, Map[TypedEvidence, SecretKey]])(implicit credentialIO: CredentialIO[F])
    extends KeyCollection[F] {

  def unlockedEntries: F[Set[TypedEvidence]] =
    r.get.map(_.keySet)

  def unlock[SK <: SecretKey: ByteCodec](evidence: TypedEvidence, password: Password): F[Option[SK]] =
    OptionT(credentialIO.unlock(evidence, password))
      .map { case (bytes, _) =>
        evidence.typePrefix match {
          case 1 => bytes.decoded[SecretKeys.Curve25519]
          case 2 => bytes.decoded[SecretKeys.Ed25519]
          case 3 => bytes.decoded[SecretKeys.ExtendedEd25519]
        }
      }
      .collect { case s: SK => s.asInstanceOf[SK] }
      .semiflatTap(sk => r.update(_.updated(evidence, sk)))
      .value

  def lift[SK <: SecretKey](evidence: TypedEvidence): F[Option[SK]] =
    OptionT(r.get.map(_.get(evidence))).collect { case s: SK => s.asInstanceOf[SK] }.value

}

object RefKeyCollection {

  def apply[F[_]: Monad: Concurrent](implicit
    credentialIO: CredentialIO[F]
  ): F[RefKeyCollection[F]] =
    Ref.of(Map.empty[TypedEvidence, SecretKey]).map(new RefKeyCollection(_))
}
