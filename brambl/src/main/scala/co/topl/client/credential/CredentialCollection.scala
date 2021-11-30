package co.topl.client.credential

import cats.data.OptionT
import cats.implicits._
import cats.{Applicative, Functor}
import co.topl.crypto.mnemonic.Bip32Indexes
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519, Password}
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.typeclasses.implicits._
import com.google.common.primitives.Longs
import scodec.{Attempt, Decoder, Encoder, Err}

/**
 * Represents a "grouping" of credentials.
 */
sealed abstract class CredentialCollection

/**
 * Represents a hierarchical credential collection derived from some root key
 *
 * HD keys
 * M / 1852' / 7091' / 0' (Account) / Role (External = 0, Internal = 1, ??? = N) / Index
 * NOTE: Keyfile must include Topl vs. Account
 */
sealed abstract class HierarchicalCredentialCollection extends CredentialCollection

object HierarchicalCredentialCollection {

  private[credential] case class SerializedBytesFormat(
    leftKey:     Sized.Strict[Bytes, SecretKeys.ExtendedEd25519.LeftLength],
    rightKey:    Sized.Strict[Bytes, SecretKeys.ExtendedEd25519.RightLength],
    chainCode:   Sized.Strict[Bytes, SecretKeys.ExtendedEd25519.ChainCodeLength],
    level1Bytes: Sized.Strict[Bytes, Lengths.`8`.type],
    level2Bytes: Sized.Strict[Bytes, Lengths.`8`.type],
    level3Bytes: Sized.Strict[Bytes, Lengths.`8`.type],
    level4Bytes: Sized.Strict[Bytes, Lengths.`8`.type]
  )

  def load[F[_]: Functor](evidence: Evidence, password: Password)(implicit
    credentialIO:                   CredentialIO[F]
  ): F[Option[HierarchicalCredentialCollection]] =
    OptionT(credentialIO.unlock(evidence, password))
      .filter(_._1.length === 128L)
      .map { case (bytes, _) =>
        SerializedBytesFormat(
          Sized.strictUnsafe(bytes.slice(0, 32)),
          Sized.strictUnsafe(bytes.slice(32, 64)),
          Sized.strictUnsafe(bytes.slice(64, 96)),
          Sized.strictUnsafe(bytes.slice(96, 104)),
          Sized.strictUnsafe(bytes.slice(104, 112)),
          Sized.strictUnsafe(bytes.slice(112, 120)),
          Sized.strictUnsafe(bytes.slice(120, 132))
        )
      }
      .filter(format =>
        format.level1Bytes.data === Bytes(Longs.toByteArray(1852L)) &&
        format.level2Bytes.data === Bytes(Longs.toByteArray(7091))
      )
      .map { format =>
        val sk = SecretKeys.ExtendedEd25519(
          format.leftKey,
          format.rightKey,
          format.chainCode
        )
        val p3 = Longs.fromByteArray(format.level3Bytes.data.toArray)
        if (p3 < 0) ToplCredentialTree(sk) else AccountCredentialTree(sk, p3)
      }
      .widen[HierarchicalCredentialCollection]
      .value
}

/**
 * An HD credential collection such that the root is a Topl root
 * i.e. `M / 1852' / 7091'`
 */
case class ToplCredentialTree(rootSK: SecretKeys.ExtendedEd25519) extends HierarchicalCredentialCollection {

  /**
   * Retrieve the credential sub-tree corresponding to the account at the given index
   */
  def account(idx: Bip32Indexes.HardenedIndex): AccountCredentialTree =
    AccountCredentialTree(ExtendedEd25519.precomputed().deriveSecret(rootSK, idx), idx.value)

  def /(idx: Bip32Indexes.HardenedIndex): AccountCredentialTree = account(idx)

}

object ToplCredentialTree {

  def nonPersisted(roleSk: SecretKeys.ExtendedEd25519): ToplCredentialTree =
    apply(roleSk)

  /**
   * Initialize and save a new ToplCredentialTree from some root key
   */
  def persisted[F[_]: Applicative](roleSk: SecretKeys.ExtendedEd25519, password: Password)(implicit
    credentialIO:                          CredentialIO[F],
    networkPrefix:                         NetworkPrefix
  ): F[ToplCredentialTree] = {
    val rawBytes =
      roleSk.leftKey.data ++
      roleSk.rightKey.data ++
      roleSk.chainCode.data ++
      Bytes(Longs.toByteArray(1852)) ++
      Bytes(Longs.toByteArray(7091)) ++
      Bytes(Longs.toByteArray(-1)) ++
      Bytes(Longs.toByteArray(-1))
    credentialIO
      .write(roleSk.dionAddress.typedEvidence.evidence, KeyFile.Metadata.ExtendedEd25519, rawBytes, password)
      .as(ToplCredentialTree(roleSk))
  }

}

/**
 * An HD credential collection such that the root is an Account _within_ a Topl root
 * i.e. `M / 1852' / 7091' / 0'`
 */
case class AccountCredentialTree(accountSK: SecretKeys.ExtendedEd25519, account: Long)
    extends HierarchicalCredentialCollection {

  def role(roleIndex: Bip32Indexes.SoftIndex): RoleCredentialList = {
    val extendedEd25519 = ExtendedEd25519.precomputed()
    val roleSk = extendedEd25519.deriveSecret(accountSK, roleIndex)

    RoleCredentialList(roleSk, roleIndex.value)
  }

  def persist[F[_]: Applicative](
    password:              Password
  )(implicit credentialIO: CredentialIO[F], networkPrefix: NetworkPrefix): F[Unit] = {
    val rawBytes =
      accountSK.leftKey.data ++
      accountSK.rightKey.data ++
      accountSK.chainCode.data ++
      Bytes(Longs.toByteArray(1852)) ++
      Bytes(Longs.toByteArray(7091)) ++
      Bytes(Longs.toByteArray(account)) ++
      Bytes(Longs.toByteArray(-1))
    credentialIO
      .write(accountSK.dionAddress.typedEvidence.evidence, KeyFile.Metadata.ExtendedEd25519, rawBytes, password)
  }

  def /(idx: Bip32Indexes.SoftIndex): RoleCredentialList = role(idx)

}

/**
 * An indexed collection of Credentials such that the root is a Role secret key
 */
case class RoleCredentialList(roleSK: SecretKeys.ExtendedEd25519, roleIndex: Long) {

  def credential(
    index:                  Bip32Indexes.SoftIndex
  )(implicit networkPrefix: NetworkPrefix, ed25519: Ed25519): Credential = {
    val extendedEd25519 = ExtendedEd25519.precomputed()
    val indexSk = extendedEd25519.deriveSecret(roleSK, index)
    implicit val encoder: Encoder[SecretKeys.ExtendedEd25519] =
      Encoder(_ => Attempt.failure(Err("Unsupported Operation: Encode derived key")))
    Credential(indexSk)
  }

  def /(idx:       Bip32Indexes.SoftIndex)(implicit
    networkPrefix: NetworkPrefix,
    ed25519:       Ed25519
  ): Credential = credential(idx)

}

/**
 * A set of unrelated keys
 */
case class CredentialSet(credentials: Map[Evidence, Credential]) extends CredentialCollection {

  def unlock[F[_]: Functor](evidence: Evidence, password: Password)(implicit
    credentialIO:                     CredentialIO[F],
    networkPrefix:                    NetworkPrefix,
    decoder:                          Decoder[Credential]
  ): F[Option[Credential]] =
    OptionT(credentialIO.unlock(evidence, password)).subflatMap { case (bytes, _) =>
      decoder.decode(bytes.toBitVector).toOption.map(_.value)
    }.value

  def withCredential[F[_]: Applicative](credential: Credential): F[CredentialSet] =
    copy(credentials = credentials.updated(credential.address.typedEvidence.evidence, credential)).pure[F]

  def withPersistentCredential[F[_]: Functor](
    credential:            Credential,
    keyType:               KeyFile.Metadata.KeyType,
    password:              Password
  )(implicit credentialIO: CredentialIO[F]): F[CredentialSet] =
    credentialIO
      .write(credential.address.typedEvidence.evidence, keyType, credential.secretData.allBytes, password)
      .as(copy(credentials = credentials.updated(credential.address.typedEvidence.evidence, credential)))

  /**
   * Remove a credential from this Set
   */
  def withoutCredential[F[_]: Applicative](evidence: Evidence): F[CredentialSet] =
    copy(credentials = credentials.removed(evidence)).pure[F]
}

object CredentialSet {

  /**
   * Create an empty credential set
   */
  val empty: CredentialSet =
    CredentialSet(Map.empty)
}
