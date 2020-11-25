package wallet

import akka.actor.{ Actor, DeadLetter, Props }

class DeadLetterListener extends Actor {
  override def receive: Receive = {
    case d: DeadLetter => println(d)
  }
}

