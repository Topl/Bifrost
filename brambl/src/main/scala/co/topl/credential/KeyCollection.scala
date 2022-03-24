package co.topl.credential

import cats._
import cats.data.OptionT
import cats.effect._
import cats.implicits._
import co.topl.crypto.signing.Password
import co.topl.models._
import co.topl.codecs.bytes.typeclasses.Persistable

//todo: JAA - I am doing something non-ideal here for testing. I want to store keyfile as json (we can also have byte persistable)
// JSON is the format that wallet and things might use, we are not necessarily tied to using JSON but it would break backwards compatibility
// so we need to determine if it is worth it
trait KeyCollection[F[_]] {
  def unlockedEntries: F[Set[TypedEvidence]]
  def unlock[SK <: SecretKey: Persistable](evidence: TypedEvidence, password: Password): F[Option[SK]]
  def lift[SK <: SecretKey](evidence:                TypedEvidence): F[Option[SK]]
}

class RefKeyCollection[F[_]: Monad](r: Ref[F, Map[TypedEvidence, SecretKey]])(implicit credentialIO: CredentialIO[F])
    extends KeyCollection[F] {

  def unlockedEntries: F[Set[TypedEvidence]] =
    r.get.map(_.keySet)

  def unlock[SK <: SecretKey: Persistable](evidence: TypedEvidence, password: Password): F[Option[SK]] =
    OptionT(credentialIO.unlock(evidence, password))
      .subflatMap { case (bytes, _) => Persistable[SK].fromPersistedBytes(bytes).toOption }
      .semiflatTap(sk => r.update(_.updated(evidence, sk)))
      .value

  // todo: JAA - this should be lifting but it looks like it is retrieving?
  def lift[SK <: SecretKey](evidence: TypedEvidence): F[Option[SK]] =
    OptionT(r.get.map(_.get(evidence))).collect { case s: SK @unchecked => s }.value

}

object RefKeyCollection {

  def apply[F[_]: Monad: Concurrent](implicit
    credentialIO: CredentialIO[F]
  ): F[RefKeyCollection[F]] =
    Ref.of(Map.empty[TypedEvidence, SecretKey]).map(new RefKeyCollection(_))
}
