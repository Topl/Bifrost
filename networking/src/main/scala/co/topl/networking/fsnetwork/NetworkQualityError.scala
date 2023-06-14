package co.topl.networking.fsnetwork

sealed abstract class NetworkQualityError extends Exception

object NetworkQualityError {
  object NoPongMessage extends NetworkQualityError
  object IncorrectPongMessage extends NetworkQualityError
}
