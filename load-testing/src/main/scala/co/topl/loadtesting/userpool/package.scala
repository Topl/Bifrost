package co.topl.loadtesting

import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.typed.{ActorRef, Behavior}
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.stream.typed.scaladsl.ActorFlow
import akka.util.Timeout
import cats.data.NonEmptyList
import co.topl.akkahttprpc.RequestModifier
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.loadtesting.user.implicits._
import co.topl.loadtesting.user.{userFlow, SendAssetsAction, SendPolysAction}
import co.topl.utils.NetworkType.NetworkPrefix

import scala.collection.immutable.ListMap
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

package object userpool {

  private val random = new Random()

  sealed abstract class Action
  case class SendAssets(action: SendAssetsAction) extends Action
  case class SendPolys(action: SendPolysAction) extends Action

  val assetsToMint = 1000
  val polysToSend = 100
  val fee = 100

  /**
   * Creates a list of actions that should be run by a looping user.
   * @param outputDirectory the directory to output results to
   * @param materializer an Akka streams materializer
   */
  def actionsList(outputDirectory: String)(implicit materializer: Materializer): NonEmptyList[Action] = {
    val successOutputPath = outputDirectory + "/successfulTxs.csv"
    val failureOutputPath = outputDirectory + "/failures.csv"
    val broadcastOutputPath = outputDirectory + "/broadcastTxs.csv"

    NonEmptyList.of(
      SendAssets(SendAssetsAction(assetsToMint, fee, successOutputPath, failureOutputPath, broadcastOutputPath)),
      SendPolys(SendPolysAction(polysToSend, fee, successOutputPath, failureOutputPath, broadcastOutputPath))
    )
  }

  /**
   * Signs a message using a keys actor on behalf of a given address.
   * @param address the address to request a signature for
   * @param keys the actor which manages the key ring
   * @param message the message to sign
   * @param timeout the amount of time to wait for a response
   * @param materializer an Akka stream materializer
   * @return a future which contains the attestation map
   */
  def signFor(address: Address, keys: ActorRef[KeysActor.Command])(message: Array[Byte])(implicit
    timeout:      Timeout,
    materializer: Materializer
  ): Future[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]] =
    Source
      .single(message)
      .via(ActorFlow.ask(keys)((message, replyTo) => KeysActor.SignMessage(address, message, replyTo)))
      .runFold(ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]()) { case (_, result) =>
        result
      }

  /**
   * Instantiates a flow which creates a collection of actor behaviors that will loop over a set of user actions.
   * @param actions the list of actions that the user will randomly execute
   * @param keys the actor managing the key ring
   * @param actorSystem the actor system to send RPC requests from
   * @param networkPrefix the prefix of the Bifrost network
   * @param requestModifier a modifier of outgoing HTTP requests
   * @return a flow which takes in a list of addresses as input and outputs looping user behaviors
   */
  def createUsersFlow(actions: NonEmptyList[Action], keys: ActorRef[KeysActor.Command])(implicit
    actorSystem:     ActorSystem,
    networkPrefix:   NetworkPrefix,
    requestModifier: RequestModifier
  ): Flow[List[Address], List[(Address, Behavior[NotUsed])], NotUsed] =
    Flow[List[Address]]
      .map(users =>
        // map each user into an actor behavior which will loop and perform random actions from the given action list
        users.map(user =>
          user ->
          LoopActor[NotUsed](
            { (_, context) =>
              implicit val keysTimeout: Timeout = 10.seconds
              implicit val materializer: Materializer = Materializer(context)
              implicit val ec: ExecutionContext = context.executionContext

              // randomly select which action to run from the list of actions
              val userFlowToRun = random.shuffle(actions.toList).head match {
                case SendPolys(a) =>
                  userFlow[SendPolysAction, SendPolysAction.Failure, SendPolysAction.Success](
                    a,
                    user,
                    users,
                    signFor(user, keys)
                  )
                case SendAssets(a) =>
                  userFlow[SendAssetsAction, SendAssetsAction.Failure, SendAssetsAction.Success](
                    a,
                    user,
                    users,
                    signFor(user, keys)
                  )
              }

              // execute the action and ignore the result
              Source.single(NotUsed).via(userFlowToRun).toMat(Sink.ignore)(Keep.right).run().map(_ => NotUsed)
            },
            _ => NotUsed
          )
        )
      )
}
