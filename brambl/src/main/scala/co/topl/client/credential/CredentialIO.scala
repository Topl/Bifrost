package co.topl.client.credential

import cats.MonadError
import cats.implicits._
import cats.effect.kernel.Sync
import co.topl.crypto.signing.Password
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Lengths, Sized}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import io.circe.syntax._

/**
 * Assumes a directory structure of
 *
 * /walletRoot/keys/_evidence_.json
 */
trait CredentialIO[F[_]] {

  def write(
    credentialSetName: String,
    evidence:          Evidence,
    keyType:           KeyFile.Metadata.KeyType,
    rawBytes:          Bytes,
    password:          Password
  ): F[Unit]

  def delete(credentialSetName: String, evidence: Evidence): F[Unit]

  def unlock(credentialSetName: String, evidence: Evidence, password: Password): F[Option[(Bytes, KeyFile.Metadata)]]

  def listCollectionNames: F[Set[String]]

  def listEvidence(collectionName: String): F[Set[Evidence]]
}

case class DiskCredentialIO[F[_]: Sync](basePath: Path) extends CredentialIO[F] {

  def write(
    credentialSetName: String,
    evidence:          Evidence,
    keyType:           KeyFile.Metadata.KeyType,
    rawBytes:          Bytes,
    password:          Password
  ): F[Unit] =
    for {
      credentialSetDir <- Paths.get(basePath.toString, credentialSetName).pure[F]
      _                <- Sync[F].blocking(Files.createDirectories(credentialSetDir))
      keyFile = KeyFile.Encryption.encrypt(rawBytes, KeyFile.Metadata(evidence, keyType), password)
      keyFileBytes <- MonadError[F, Throwable].fromEither(Bytes.encodeUtf8(keyFile.asJson.toString()))
      keyFilePath = Paths.get(credentialSetDir.toString, evidence.data.toBase58 + ".json")
      _ <- Sync[F].blocking(Files.write(keyFilePath, keyFileBytes.toArray))
    } yield ()

  def delete(credentialSetName: String, evidence: Evidence): F[Unit] =
    Sync[F].blocking {
      val credentialSetDir = Paths.get(basePath.toString, credentialSetName)
      val credentialFile = Paths.get(credentialSetDir.toString, evidence.data.toBase58 + ".json")
      Files.deleteIfExists(credentialFile)
    }

  def unlock(credentialSetName: String, evidence: Evidence, password: Password): F[Option[(Bytes, KeyFile.Metadata)]] =
    Sync[F].blocking {
      val keyFilePath = Paths.get(basePath.toString, credentialSetName, s"${evidence.data.toBase58}.json")
      Option
        .when(Files.exists(keyFilePath) && Files.isRegularFile(keyFilePath))(
          Files.readString(keyFilePath, StandardCharsets.UTF_8)
        )
        .flatMap(io.circe.parser.parse(_).flatMap(_.as[KeyFile]).toOption)
        .flatMap(keyFile => KeyFile.Encryption.decrypt(keyFile, password).toOption.map((_, keyFile.metadata)))
    }

  def listCollectionNames: F[Set[String]] =
    Sync[F].blocking {
      import scala.jdk.StreamConverters._
      Files.list(basePath).toScala(Seq).map(_.getFileName.toString).toSet
    }

  def listEvidence(collectionName: String): F[Set[Evidence]] =
    Sync[F].blocking {
      import scala.jdk.StreamConverters._
      Files
        .list(Paths.get(basePath.toString, collectionName))
        .toScala(Seq)
        .map(_.getFileName.toString)
        .filter(_.endsWith(".json"))
        .map(_.dropRight(5))
        .flatMap(Bytes.fromBase58(_))
        .map(allBytes => Sized.strictUnsafe[Bytes, Lengths.`32`.type](allBytes))
        .toSet
    }
}
