package co.topl.interpreters

import akka.Done
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, DispatcherSelector}
import akka.util.Timeout
import cats.data.Chain
import cats.effect.kernel.{Async, Sync}
import cats.implicits._
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.algebras.SecureStore
import co.topl.interpreters.AkkaSecureStoreActor.ReceivableMessages
import co.topl.models.Bytes

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

class AkkaSecureStore[F[_]: Async](actorRef: ActorRef[AkkaSecureStoreActor.ReceivableMessage])(implicit
  system:                                    ActorSystem[_],
  timeout:                                   Timeout
) extends SecureStore[F] {

  def write[A: Persistable](name: String, data: A): F[Unit] =
    ask[Done](AkkaSecureStoreActor.ReceivableMessages.Write(name, data, _)).void

  def consume[A: Persistable](name: String): F[Option[A]] =
    ask(ReceivableMessages.Consume[A](name, _))

  def list: F[Chain[String]] =
    ask(AkkaSecureStoreActor.ReceivableMessages.List)

  def erase(name: String): F[Unit] =
    ask[Done](AkkaSecureStoreActor.ReceivableMessages.Erase(name, _)).void

  private def ask[Res](message: ActorRef[Res] => AkkaSecureStoreActor.ReceivableMessage): F[Res] =
    Async[F]
      .fromFuture(
        Sync[F].delay(actorRef.ask[Res](message))
      )
}

object AkkaSecureStore {

  object Eval {

    def make[F[_]: Async](baseDir: Path)(implicit system: ActorSystem[_], timeout: Timeout): F[AkkaSecureStore[F]] = {
      val actorName = {
        val sanitizedPath =
          baseDir.toString
            .replace('/', '-')
            .filter(('a' to 'z').toSet ++ ('A' to 'Z') ++ ('0' to '9') ++ Set('-'))
        s"akka-secure-store-$sanitizedPath"
      }
      Async[F]
        .delay(
          system.systemActorOf(
            AkkaSecureStoreActor(baseDir),
            actorName,
            DispatcherSelector.blocking()
          )
        )
        .map(new AkkaSecureStore[F](_))
    }
  }
}

object AkkaSecureStoreActor {
  import scala.jdk.CollectionConverters._

  def apply(baseDir: Path): Behavior[ReceivableMessage] =
    Behaviors.receiveMessage {
      case ReceivableMessages.List(replyTo) =>
        val names =
          Chain
            .fromSeq(
              Files
                .list(baseDir)
                .iterator()
                .asScala
                .toSeq
            )
            .filter(Files.isRegularFile(_))
            .map(_.getFileName.toString)
        replyTo.tell(names)
        Behaviors.same
      case w: ReceivableMessages.Write[_] =>
        erase(w.name, baseDir)
        w.run(baseDir)
        Behaviors.same
      case c: ReceivableMessages.Consume[_] =>
        c.run(baseDir)
        erase(c.name, baseDir)
        Behaviors.same
      case ReceivableMessages.Erase(name, replyTo) =>
        erase(name, baseDir)
        replyTo.tell(Done)
        Behaviors.same
    }

  private def erase(name: String, baseDir: Path): Unit = {
    val path = Paths.get(baseDir.toString, name)
    if (Files.exists(path) && Files.isRegularFile(path)) {
      val size = Files.size(path).toInt
      Files.write(path, Array.fill[Byte](size)(0), StandardOpenOption.TRUNCATE_EXISTING)
      Files.delete(path)
    }
  }

  sealed abstract class ReceivableMessage

  object ReceivableMessages {
    case class List(replyTo: ActorRef[Chain[String]]) extends ReceivableMessage

    case class Write[A: Persistable](name: String, data: A, replyTo: ActorRef[Done]) extends ReceivableMessage {

      private[AkkaSecureStoreActor] def run(baseDir: Path): Unit = {
        val path = Paths.get(baseDir.toString, name)
        Files.write(path, data.persistedBytes.toArray)
        replyTo.tell(Done)
      }
    }

    case class Consume[A: Persistable](name: String, replyTo: ActorRef[Option[A]]) extends ReceivableMessage {

      private[AkkaSecureStoreActor] def run(baseDir: Path): Unit = {
        val path = Paths.get(baseDir.toString, name)
        if (Files.exists(path) && Files.isRegularFile(path)) {
          val bytes = Bytes(Files.readAllBytes(path))
          replyTo.tell(bytes.decodePersisted[A].toOption)
        } else {
          replyTo.tell(None)
        }
      }
    }
    case class Erase(name: String, replyTo: ActorRef[Done]) extends ReceivableMessage
  }
}
