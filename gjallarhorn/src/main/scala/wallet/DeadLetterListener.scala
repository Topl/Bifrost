package wallet

import akka.actor.{ Actor, DeadLetter }

/**
  * Actor used for debugging when dead letters are received.
  */
class DeadLetterListener extends Actor {
  override def receive: Receive = {
    case d: DeadLetter => println(d)
  }
}

