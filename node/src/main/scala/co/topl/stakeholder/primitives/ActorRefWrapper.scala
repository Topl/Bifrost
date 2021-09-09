package co.topl.stakeholder.primitives

import akka.pattern.ask
import akka.util.Timeout
import co.topl.stakeholder.cases.MessageFromLocalToRemote

import scala.concurrent.Future

/**
 * AMS 2020:
 * Wrapper class for akka ActorRef, remote and local actors handled accordingly
 * Router ref is the interface with network controller for sending messages to remote
 * A handy way of treating actor paths from remote and local on the same footing
 */

case class ActorRefWrapper(
  inputRouterRef: akka.actor.ActorRef,
  actorRef:       akka.actor.ActorRef,
  actorPath:      akka.actor.ActorPath,
  remote:         Boolean = false
) {

  def canEqual(a: Any): Boolean = a.isInstanceOf[ActorRefWrapper]

  override def equals(that: Any): Boolean = that match {
    case that: ActorRefWrapper =>
      that.canEqual(this) &&
        this.actorRef == that.actorRef &&
        this.actorPath == that.actorPath &&
        this.inputRouterRef == that.inputRouterRef &&
        this.remote == that.remote
    case _ => false
  }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (actorRef != null) 0 else actorRef.hashCode)
    result = prime * result + (if (actorPath != null) 0 else actorPath.hashCode)
    result = prime * result + (if (inputRouterRef != null) 0 else inputRouterRef.hashCode)
    result = prime * result + remote.hashCode
    result
  }

  def path: akka.actor.ActorPath = actorPath

  override def toString: String = path.toString

  def !(that: Any)(implicit
    sender:   akka.actor.ActorRef = akka.actor.Actor.noSender
  ): Unit = if (this.remote) {
    implicit val routerRef: ActorRefWrapper = ActorRefWrapper.routerRef(inputRouterRef)
    inputRouterRef ! MessageFromLocalToRemote(ActorRefWrapper(sender), actorPath, that)
  } else {
    this.actorRef ! that
  }

  def ?(that: Any)(implicit
    timeout:  Timeout,
    sender:   akka.actor.ActorRef = akka.actor.Actor.noSender
  ): Future[Any] = if (this.remote) {
    implicit val routerRef: ActorRefWrapper = ActorRefWrapper.routerRef(inputRouterRef)
    inputRouterRef ? MessageFromLocalToRemote(ActorRefWrapper(sender), actorPath, that)
  } else {
    this.actorRef ? that
  }

}

object ActorRefWrapper {

  def apply(actorRef: akka.actor.ActorRef)(implicit routerRef: ActorRefWrapper): ActorRefWrapper =
    new ActorRefWrapper(
      actorRef = actorRef,
      actorPath = actorRef.path,
      remote = false,
      inputRouterRef = routerRef.actorRef
    )

  def apply(path: akka.actor.ActorPath)(implicit routerRef: ActorRefWrapper): ActorRefWrapper =
    new ActorRefWrapper(
      actorPath = path,
      inputRouterRef = routerRef.actorRef,
      remote = true,
      actorRef = routerRef.actorRef
    )

  def routerRef(actorRef: akka.actor.ActorRef) = new ActorRefWrapper(actorRef, actorRef, actorRef.path, false)

}
