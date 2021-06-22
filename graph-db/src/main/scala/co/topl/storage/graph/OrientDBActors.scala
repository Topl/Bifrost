package co.topl.storage.graph

import akka.{Done, NotUsed}
import akka.actor.typed.{ActorRef, Behavior, DispatcherSelector, PostStop, Terminated}
import akka.actor.typed.scaladsl.Behaviors
import akka.pattern.StatusReply
import akka.stream.scaladsl.{Sink, Source}
import co.topl.storage.iteratorSourceOnDispatcher
import com.tinkerpop.blueprints.impls.orient.{OrientBaseGraph, OrientGraphFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

private[graph] object OrientDBGraphActorParent {

  def apply(factory: OrientGraphFactory): Behavior[Message] = {
    def withState(
      readyForWork:   List[ActorRef[OrientDBGraphActor.Message[_]]],
      readyForTxWork: List[ActorRef[OrientDbTransactionActor.Message]]
    ): Behavior[Message] =
      Behaviors.receive((ctx, message) =>
        message match {
          case GiveWork(to) =>
            withState(to +: readyForWork, readyForTxWork)
          case Work(message) =>
            val (worker, newReadyForWork) =
              readyForWork match {
                case Nil =>
                  val w =
                    ctx.spawnAnonymous(
                      OrientDBGraphActor(() => factory.getNoTx, ctx.self),
                      DispatcherSelector.fromConfig("orient-db-actor-dispatcher")
                    )
                  (w, readyForWork)
                case w :: rest =>
                  (w, rest)
              }

            worker ! message

            withState(newReadyForWork, readyForTxWork)

          case GiveTxWork(to) =>
            withState(readyForWork, to +: readyForTxWork)

          case TxWork(message) =>
            val (txWorker, newReadyForTxWork) =
              readyForTxWork match {
                case Nil =>
                  val w =
                    ctx.spawnAnonymous(
                      OrientDbTransactionActor(factory, ctx.self),
                      DispatcherSelector.fromConfig("orient-db-actor-dispatcher")
                    )
                  (w, readyForTxWork)
                case w :: rest =>
                  (w, rest)
              }

            txWorker ! message

            withState(readyForWork, newReadyForTxWork)
          case OrientDBGraphActorParent.Stop(replyTo) =>
            val children = ctx.children.toList
            if (children.nonEmpty) {
              children.foreach(ctx.watch)
              children.foreach(ctx.stop)
              stopping(replyTo)
            } else {
              replyTo ! Done
              Behaviors.stopped
            }
        }
      )

    def stopping(replyTo: ActorRef[Done]): Behavior[OrientDBGraphActorParent.Message] =
      Behaviors.receiveSignal { case (ctx, Terminated(_)) =>
        if (ctx.children.nonEmpty) Behaviors.same
        else {
          replyTo ! Done
          Behaviors.stopped
        }
      }
    withState(Nil, Nil)
  }

  sealed abstract class Message
  case class GiveWork(to: ActorRef[OrientDBGraphActor.Message[_]]) extends Message
  case class GiveTxWork(to: ActorRef[OrientDbTransactionActor.Message]) extends Message
  case class Work[Response](message: OrientDBGraphActor.Message[Response]) extends Message
  case class TxWork[Response](message: OrientDbTransactionActor.Message) extends Message
  case class Stop(replyTo: ActorRef[Done]) extends Message
}

private[graph] object OrientDBGraphActor {

  def apply(factory: () => OrientBaseGraph, parent: ActorRef[OrientDBGraphActorParent.Message]): Behavior[Message[_]] =
    Behaviors.setup { ctx =>
      import ctx.executionContext
      def idle: Behavior[Message[_]] =
        Behaviors.receiveMessagePartial {
          case e: Execute[_] =>
            val session = factory()
            ctx.pipeToSelf(e.runAndReply(session, ctx.executionContext))(CompleteExecution(_))
            busy(session)
          case e: ExecuteIterator[_] =>
            val session = factory()
            e.runAndReply(session, ctx.self, ctx.executionContext)
            Behaviors.same
            busy(session)
        }

      def busy(session: OrientBaseGraph): Behavior[Message[_]] =
        Behaviors
          .receiveMessagePartial[Message[_]] { case _: CompleteExecution[_] =>
            parent.tell(OrientDBGraphActorParent.GiveWork(ctx.self))
            session.shutdown()
            idle
          }
          .receiveSignal { case (_, PostStop) =>
            session.shutdown()
            Behaviors.same
          }

      idle
    }

  sealed abstract class Message[+Response]

  case class Execute[T](f: OrientBaseGraph => T, replyTo: ActorRef[StatusReply[T]]) extends Message {

    def runAndReply(session: OrientBaseGraph, blockingEc: ExecutionContext)(implicit ec: ExecutionContext): Future[T] =
      Future(f(session))(blockingEc)
        .andThen {
          case Success(v)         => replyTo.tell(StatusReply.Success(v))
          case Failure(exception) => replyTo ! StatusReply.Error(exception)
        }
  }

  case class ExecuteIterator[T](f: OrientBaseGraph => Iterator[T], replyTo: ActorRef[Source[T, NotUsed]])
      extends Message {

    def runAndReply(
      session:                  OrientBaseGraph,
      worker:                   ActorRef[OrientDBGraphActor.Message[_]],
      blockingExecutionContext: ExecutionContext
    ): Unit = {
      val source: Source[T, NotUsed] =
        iteratorSourceOnDispatcher(() => f(session), blockingExecutionContext)
          .alsoTo(Sink.onComplete(result => worker ! CompleteExecution(result)))
      replyTo.tell(source)
    }
  }

  case class CompleteExecution[T](result: Try[T]) extends Message

}

private[graph] object OrientDbTransactionActor {

  def apply(factory: OrientGraphFactory, parent: ActorRef[OrientDBGraphActorParent.Message]): Behavior[Message] =
    Behaviors.setup { ctx =>
      val session = factory.getTx
      session.setAutoStartTx(false)

      Behaviors
        .receiveMessage[Message] {
          case Execute(f, replyTo) =>
            session.begin()
            val computation: Future[Any] = f(session, ctx.executionContext)
            ctx.pipeToSelf(computation)(CompleteExecution(_, replyTo))
            Behaviors.same
          case CompleteExecution(result, replyTo) =>
            result.flatMap(r => Try(session.commit()).map(_ => r)) match {
              case Failure(exception) =>
                session.rollback()
                replyTo ! StatusReply.Error(exception)
              case Success(value) =>
                replyTo ! StatusReply.Success(value)
            }
            parent.tell(OrientDBGraphActorParent.GiveTxWork(ctx.self))
            Behaviors.same
        }
        .receiveSignal { case (_, PostStop) =>
          session.shutdown()
          Behaviors.stopped[Message]
        }
    }

  sealed abstract class Message

  case class Execute[T](f: (OrientBaseGraph, ExecutionContext) => Future[T], replyTo: ActorRef[StatusReply[T]])
      extends Message
  case class CompleteExecution[T](result: Try[T], replyTo: ActorRef[StatusReply[T]]) extends Message
}
