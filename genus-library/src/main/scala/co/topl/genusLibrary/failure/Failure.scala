package co.topl.genusLibrary.failure

sealed abstract case class Failure(message: String, exception: Option[Exception])

object Failures {}
