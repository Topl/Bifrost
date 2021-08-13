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
import co.topl.loadtesting.userpool.UserPoolActor

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

  /**
   * Instantiates a new simulation behavior accepting commands messages of type `Command`.
   * @param statsFile the statistics file to output results to
   * @param networkPrefix the Bifrost network prefix
   * @param timeout the amount of time to wait for requests between actors
   * @param requestModifier the HTTP request modifier for sending RPC messages to Bifrost
   * @return an instance of `Behavior[Command]`
   */
  def apply(statsFile: String)(implicit
    networkPrefix:     NetworkPrefix,
    timeout:           Timeout,
    requestModifier:   RequestModifier
  ): Behavior[Command] =
    Behaviors.setup { context =>
      val keys = context.spawn(KeysActor(), "keys")
      val users = context.spawn(UserPoolActor(keys, statsFile), "userpool")

      withState(keys, users)
    }

  /**
   * Instantiates a simulation actor behavior with the given state.
   * @param keys the keys actor which manages the key ring
   * @param users the user pool actor which manages running users
   * @param timeout the amount of time to wait for actor responses
   * @return an actor behavior of type `Behavior[Command]`
   */
  def withState(keys: ActorRef[KeysActor.Command], users: ActorRef[UserPoolActor.Command])(implicit
    timeout:          Timeout
  ): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      implicit val materializer: Materializer = Materializer(context)

      message match {
        case AddUsers(numUsers, seed) =>

          /**
           * Flow to create new keys in the key ring and get back the created addresses.
           */
        val createKeysFlow: Flow[Any, Try[Set[Address]], NotUsed] =
            ActorFlow.ask(keys)((_: Any, replyTo: ActorRef[Try[Set[Address]]]) =>
              KeysActor.GenerateKeyPairs(seed, numUsers, replyTo)
            )

          /**
           * Flow to add new users to the user pool from a given set of addresses.
           */
          val addUsersSink: Sink[Try[Set[Address]], Future[Done]] =
            Sink.foreach(addresses => users ! UserPoolActor.AddUsers(addresses.get))

          Source
            .single(NotUsed)
            // ask keys actor to generate new addresses
            .via(createKeysFlow)
            // send new addresses to the user pool to create new users
            .to(addUsersSink)
            .run()

          Behaviors.same

        case Stop =>
          context.log.info("stopping simulation")
          Behaviors.stopped
      }
    }
}
