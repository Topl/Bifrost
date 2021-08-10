package co.topl.loadtesting

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.typed.scaladsl.ActorFlow
import akka.util.Timeout
import akka.{Done, NotUsed}
import co.topl.akkahttprpc.RequestModifier
import co.topl.attestation.Address
import co.topl.utils.NetworkType.NetworkPrefix

import scala.concurrent.Future
import scala.util.Try

object SimulationActor {
  sealed trait Command

  case class AddUsers(numUsers: Int, seed: String) extends Command

  case object Stop extends Command

  def apply(statsFile: String)(implicit
    networkPrefix:   NetworkPrefix,
    timeout:         Timeout,
    requestModifier: RequestModifier
  ): Behavior[Command] =
    Behaviors.setup { context =>
      val keys = context.spawn(KeysActor(), "keys")
      val users = context.spawn(UserPoolActor(keys, statsFile), "userpool")

      update(keys, users)
    }

  def update(keys: ActorRef[KeysActor.Command], users: ActorRef[UserPoolActor.Command])(implicit
    timeout:       Timeout
  ): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      implicit val materializer: Materializer = Materializer(context)

      message match {
        case AddUsers(numUsers, seed) =>

          val createKeysFlow: Flow[Any, Try[Set[Address]], NotUsed] =
            ActorFlow.ask(keys)((_: Any, replyTo: ActorRef[Try[Set[Address]]]) =>
              KeysActor.GenerateKeyPairs(seed, numUsers, replyTo)
            )

          val addUsersSink: Sink[Try[Set[Address]], Future[Done]] =
            Sink.foreach(addresses => users ! UserPoolActor.AddUsers(addresses.get))

          Source
            .single(NotUsed)
            .via(createKeysFlow)
            .to(addUsersSink)
            .run()

          Behaviors.same

        case Stop =>
          context.log.info("stopping simulation")
          Behaviors.stopped
      }
    }
}
