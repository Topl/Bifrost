package co.topl.client.credential

import cats.MonadError
import cats.implicits._
import cats.effect.kernel.Sync
import co.topl.crypto.signing.Password
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import io.circe.syntax._

/**
 * Assumes a directory structure of
 *
 * /walletRoot/credentials/_credentialCollectionName_/_address_.json
 */
trait CredentialIO[F[_]] {
  def write(credentialSetName: String, address: DionAddress, rawBytes: Bytes, password: Password): F[Unit]

  def delete(credentialSetName: String, address: DionAddress): F[Unit]

  def unlock(credentialSetName: String, address: DionAddress, password: Password): F[Option[Bytes]]

  def listCollectionNames: F[Set[String]]

  def listAddresses(collectionName: String): F[Set[DionAddress]]
}

case class DiskCredentialIO[F[_]: Sync](basePath: Path) extends CredentialIO[F] {

  def write(credentialSetName: String, address: DionAddress, rawBytes: Bytes, password: Password): F[Unit] =
    for {
      credentialSetDir <- Paths.get(basePath.toString, credentialSetName).pure[F]
      _                <- Sync[F].blocking(Files.createDirectories(credentialSetDir))
      keyFile = KeyFile.Encryption.encrypt(rawBytes, address, password)
      keyFileBytes <- MonadError[F, Throwable].fromEither(Bytes.encodeUtf8(keyFile.asJson.toString()))
      keyFilePath = Paths.get(credentialSetDir.toString, keyFile.address + ".json")
      _ <- Sync[F].blocking(Files.write(keyFilePath, keyFileBytes.toArray))
    } yield ()

  def delete(credentialSetName: String, address: DionAddress): F[Unit] =
    Sync[F].blocking {
      val credentialSetDir = Paths.get(basePath.toString, credentialSetName)
      val credentialFile = Paths.get(credentialSetDir.toString, address.allBytes.toBase58 + ".json")
      Files.deleteIfExists(credentialFile)
    }

  def unlock(credentialSetName: String, address: DionAddress, password: Password): F[Option[Bytes]] =
    Sync[F].blocking {
      val keyFilePath = Paths.get(basePath.toString, credentialSetName, s"${address.allBytes.toBase58}.json")
      Option
        .when(Files.exists(keyFilePath) && Files.isRegularFile(keyFilePath))(
          Files.readString(keyFilePath, StandardCharsets.UTF_8)
        )
        .flatMap(io.circe.parser.parse(_).flatMap(_.as[KeyFile]).toOption)
        .flatMap(KeyFile.Encryption.decrypt(_, password).toOption)
    }

  def listCollectionNames: F[Set[String]] =
    Sync[F].blocking {
      import scala.jdk.StreamConverters._
      Files.list(basePath).toScala(Seq).map(_.getFileName.toString).toSet
    }

  def listAddresses(collectionName: String): F[Set[DionAddress]] =
    Sync[F].blocking {
      import scala.jdk.StreamConverters._
      Files
        .list(Paths.get(basePath.toString, collectionName))
        .toScala(Seq)
        .map(_.getFileName.toString)
        .filter(_.endsWith(".json"))
        .map(_.dropRight(5))
        .flatMap(Bytes.fromBase58(_))
        .map(allBytes =>
          DionAddress(NetworkPrefix(allBytes(0)), TypedEvidence(allBytes(1), Sized.strictUnsafe(allBytes.drop(2))))
        )
        .toSet
    }
}
