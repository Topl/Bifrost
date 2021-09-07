package wallet

/**
 * Actor used for debugging when dead letters are received.
 */
class DeadLetterListener extends Actor with Logging {

  override def receive: Receive = { case d: DeadLetter =>
    log.debug(d.toString)
  }
}
