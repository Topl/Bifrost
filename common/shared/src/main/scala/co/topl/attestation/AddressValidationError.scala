package co.topl.attestation

sealed abstract class AddressValidationError

object AddressValidationErrors {
  case object InvalidNetworkPrefix extends AddressValidationError
  case object InvalidAddress extends AddressValidationError
  case object NetworkTypeMismatch extends AddressValidationError
  case object InvalidAddressLength extends AddressValidationError
  case object InvalidChecksum extends AddressValidationError
  case class AddressDecodeFailure(message: String) extends AddressValidationError
}
