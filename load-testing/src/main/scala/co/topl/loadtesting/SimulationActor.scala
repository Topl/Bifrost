package co.topl.loadtesting

import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
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

/**
 * Guardian actor of the load testing user simulation.
 */
object SimulationActor {
  sealed trait Command

  /**
   * Adds a number of new users to the simulation.
   * @param numUsers the number of users to add
   * @param seed a seed to deterministically generate users from
   */
  case class AddUsers(numUsers: Int, seed: String) extends Command

  /**
   * Stops the execution of the simulation and shuts down the actor.
   */
  case object Stop extends Command

  private case class SpawnUsers(users: List[(Address, Behavior[NotUsed])]) extends Command

  /**
   * Instantiates a new simulation behavior accepting commands messages of type `Command`.
   * @param statsFile the statistics file to output results to
   * @param networkPrefix the Bifrost network prefix
   * @param timeout the amount of time to wait for requests between actors
   * @param requestModifier the HTTP request modifier for sending RPC messages to Bifrost
   * @return an instance of `Behavior[Command]`
   */
  def apply(outputDirectory: String)(implicit
    networkPrefix:   NetworkPrefix,
    timeout:         Timeout,
    requestModifier: RequestModifier
  ): Behavior[Command] =
    Behaviors.setup { context =>
      val keys = context.spawn(KeysActor(), "keys")

      implicit val actorSystem = context.system.classicSystem

      withState(outputDirectory, keys)
    }

  /**
   * Instantiates a simulation actor behavior with the given state.
   * @param keys the keys actor which manages the key ring
   * @param users the user pool actor which manages running users
   * @param timeout the amount of time to wait for actor responses
   * @return an actor behavior of type `Behavior[Command]`
   */
  def withState(outputDirectory: String, keys: ActorRef[KeysActor.Command])(implicit
    timeout:         Timeout,
    actorSystem:     ActorSystem,
    networkPrefix:   NetworkPrefix,
    requestModifier: RequestModifier
  ): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      implicit val materializer: Materializer = Materializer(context)
      implicit val currentContext: ActorContext[Command] = context

      message match {
        case AddUsers(numUsers, seed) =>
          Source
            .single(NotUsed)
            // ask keys actor to generate new addresses
            .via(
              ActorFlow.ask(keys)((_: Any, replyTo: ActorRef[Try[Set[Address]]]) =>
                KeysActor.GenerateKeyPairs(seed, numUsers, replyTo)
              )
            )
            // send new addresses to the user pool to create new users
            .map(_.get.toList)
            .via(userpool.createUsersFlow(userpool.actionsList(outputDirectory), keys))
            .runForeach(users => context.self ! SpawnUsers(users))

          Behaviors.same

        case SpawnUsers(users) =>
          users.foreach { user =>
            val newUser = context.spawn(user._2, s"User_${user._1}")

            newUser ! NotUsed
          }

          Behaviors.same

        case Stop =>
          context.log.info("stopping simulation")
          Behaviors.stopped
      }
    }
}
