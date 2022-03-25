package co.topl.credential

import cats.MonadError
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.codecs._
import co.topl.codecs.binary.typeclasses.Persistable
import co.topl.crypto.signing.Password
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Lengths, Sized}
import co.topl.typeclasses.ContainsEvidence
import co.topl.typeclasses.implicits._
import io.circe.syntax._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/**
 * Assumes a directory structure of
 *
 * /walletRoot/keys/_evidence_.json
 */
trait CredentialIO[F[_]] {

  def write(
    evidence: TypedEvidence,
    rawBytes: Bytes,
    password: Password
  ): F[Unit]

  def delete(evidence: TypedEvidence): F[Unit]

  def unlock(evidence: TypedEvidence, password: Password): F[Option[(Bytes, KeyFile.Metadata)]]

  def listEvidence: F[Set[TypedEvidence]]
}

object CredentialIO {

  trait Instances {

    implicit class TOps[T](t: T) {

      def save[F[_]](
        password:                  Password
      )(implicit containsEvidence: ContainsEvidence[T], codec: Persistable[T], credentialIO: CredentialIO[F]): F[Unit] =
        credentialIO.write(t.typedEvidence, Bytes(t.persistedBytes), password)
    }
  }
}

case class DiskCredentialIO[F[_]: Sync](basePath: Path) extends CredentialIO[F] {

  def write(
    evidence: TypedEvidence,
    rawBytes: Bytes,
    password: Password
  ): F[Unit] =
    for {
      _ <- Sync[F].blocking(Files.createDirectories(basePath))
      keyFile = KeyFile.Encryption.encrypt(rawBytes, KeyFile.Metadata(evidence), password)
      keyFileBytes <- MonadError[F, Throwable].fromEither(Bytes.encodeUtf8(keyFile.asJson.toString()))
      keyFilePath = Paths.get(basePath.toString, evidence.allBytes.toBase58 + ".json")
      _ <- Sync[F].blocking(Files.write(keyFilePath, keyFileBytes.toArray))
    } yield ()

  def delete(evidence: TypedEvidence): F[Unit] =
    Sync[F].blocking {
      val credentialFile = Paths.get(basePath.toString, evidence.allBytes.toBase58 + ".json")
      Files.deleteIfExists(credentialFile)
    }

  def unlock(evidence: TypedEvidence, password: Password): F[Option[(Bytes, KeyFile.Metadata)]] =
    Sync[F].blocking {
      val keyFilePath = Paths.get(basePath.toString, s"${evidence.allBytes.toBase58}.json")
      if (Files.exists(keyFilePath) && Files.isRegularFile(keyFilePath))
        Some(Files.readString(keyFilePath, StandardCharsets.UTF_8))
          .flatMap(io.circe.parser.parse(_).flatMap(_.as[KeyFile]).toOption)
          .flatMap(keyFile => KeyFile.Encryption.decrypt(keyFile, password).toOption.map((_, keyFile.metadata)))
      else
        None
    }

  def listEvidence: F[Set[TypedEvidence]] =
    Sync[F].blocking {
      val b = scala.collection.mutable.Buffer.empty[Path]
      Files
        .list(basePath)
        .forEach(p => b.append(p))
      b
        .map(_.getFileName.toString)
        .filter(_.endsWith(".json"))
        .map(_.dropRight(5))
        .flatMap(Bytes.fromBase58(_))
        .map(allBytes => TypedEvidence(allBytes.head, Sized.strictUnsafe[Bytes, Lengths.`32`.type](allBytes.tail)))
        .toSet
    }
}
