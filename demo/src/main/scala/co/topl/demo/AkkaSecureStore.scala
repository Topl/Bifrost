package co.topl.demo

import akka.Done
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, DispatcherSelector}
import akka.util.Timeout
import cats.Id
import cats.data.Chain
import cats.effect.kernel.{Async, Sync}
import cats.implicits._
import co.topl.crypto.keyfile.{SecureBytes, SecureData, SecureStore}

import java.io.BufferedWriter
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.util.chaining._

class AkkaSecureStore[F[_]: Async](actorRef: ActorRef[AkkaSecureStoreActor.ReceivableMessage])(implicit
  system:                                    ActorSystem[_],
  timeout:                                   Timeout
) extends SecureStore[F] {

  def write(data: SecureData): F[Unit] =
    ask[Done](AkkaSecureStoreActor.ReceivableMessages.Write(data, _)).void

  def read(name: String): F[Option[SecureData]] =
    ask(AkkaSecureStoreActor.ReceivableMessages.Read(name, _))

  def list: F[Chain[String]] =
    ask(AkkaSecureStoreActor.ReceivableMessages.List)

  def delete(name: String): F[Unit] =
    ask[Done](AkkaSecureStoreActor.ReceivableMessages.Delete(name, _)).void

  private def ask[Res](message: ActorRef[Res] => AkkaSecureStoreActor.ReceivableMessage): F[Res] =
    Async[F]
      .fromFuture(
        Sync[F].delay(actorRef.ask[Res](message))
      )
}

object AkkaSecureStore {

  object Eval {

    def make[F[_]: Async](basePath: Path)(implicit system: ActorSystem[_], timeout: Timeout): F[AkkaSecureStore[F]] = {
      val actorName = {
        val sanitizedPath =
          basePath.toString
            .replace('/', '-')
            .filter(('a' to 'z').toSet ++ ('A' to 'Z') ++ ('0' to '9') ++ Set('-'))
        s"akka-secure-store-$sanitizedPath"
      }
      Async[F]
        .delay(
          system.systemActorOf(
            AkkaSecureStoreActor(AkkaSecureStoreActor.State(Map.empty, basePath)),
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

  def apply(state: State): Behavior[ReceivableMessage] =
    Behaviors.receiveMessage {
      case ReceivableMessages.List(replyTo) =>
        val names =
          Chain
            .fromSeq(
              Files
                .list(state.basePath)
                .iterator()
                .asScala
                .toSeq
            )
            .filter(Files.isRegularFile(_))
            .map(_.getFileName.toString)
        replyTo.tell(names)
        Behaviors.same
      case ReceivableMessages.Write(data, replyTo) =>
        val erasedState = deleteImpl(data.name, state)
        val path = Paths.get(erasedState.basePath.toString, data.name)
        data.bytes
          .foldLeft[Id, BufferedWriter](Files.newBufferedWriter(path))((writer, byte) => writer.tap(_.write(byte)))(
            writer => writer.close().pure[Id]
          )
        val newState = erasedState.copy(entries = erasedState.entries.updated(data.name, data.bytes))
        replyTo.tell(Done)
        apply(newState)
      case ReceivableMessages.Read(name, replyTo) =>
        state.entries.get(name) match {
          case Some(value) =>
            replyTo.tell(Some(SecureData(name, value)))
            Behaviors.same
          case None =>
            val path = Paths.get(state.basePath.toString, name)
            if (Files.exists(path) && Files.isRegularFile(path)) {
              val secureData = SecureData(name, SecureBytes(Files.readAllBytes(path)))
              val newEntries = state.entries.updated(name, secureData.bytes)
              replyTo.tell(Some(secureData))
              apply(state.copy(entries = newEntries))
            } else {
              replyTo.tell(None)
              Behaviors.same
            }
        }
      case ReceivableMessages.Delete(name, replyTo) =>
        val newState = deleteImpl(name, state)
        replyTo.tell(Done)
        apply(newState)
    }

  private def deleteImpl(name: String, state: State): State = {
    val newEntries = state.entries
      .get(name)
      .fold(state.entries) { bytes =>
        bytes.erase()
        state.entries.removed(name)
      }
    val path = Paths.get(state.basePath.toString, name)
    if (Files.exists(path) && Files.isRegularFile(path)) {
      Files.write(path, Array.fill[Byte](Files.size(path).toInt)(0), StandardOpenOption.TRUNCATE_EXISTING)
      Files.delete(path)
    }
    state.copy(entries = newEntries)
  }

  sealed abstract class ReceivableMessage

  object ReceivableMessages {
    case class List(replyTo: ActorRef[Chain[String]]) extends ReceivableMessage
    case class Write(data: SecureData, replyTo: ActorRef[Done]) extends ReceivableMessage
    case class Read(name: String, replyTo: ActorRef[Option[SecureData]]) extends ReceivableMessage
    case class Delete(name: String, replyTo: ActorRef[Done]) extends ReceivableMessage
  }

  case class State(entries: Map[String, SecureBytes], basePath: Path)
}
