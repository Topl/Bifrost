package co.topl.attestation.ops

import cats.Semigroup
import cats.data.{Validated, ValidatedNec}
import cats.implicits._
import co.topl.attestation.{Address, AddressValidationError, AddressValidationErrors}
import co.topl.codecs.binary.typeclasses.implicits._
import co.topl.utils.NetworkType
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.{Base16Data, Base58Data}

import scala.language.implicitConversions

class AttestationBytesOps(private val value: Array[Byte]) extends AnyVal {
  import AttestationBytesOps.byteArraySemigroup

  def decodeAddress(implicit networkPrefix: NetworkPrefix): ValidatedNec[AddressValidationError, Address] =
    networkPrefixValidation(value)
      .map(_ => value)
      .combine(lengthValidation(value))
      .combine(checksumValidation(value))
      .andThen(_ => parseValidation(value))

  private[attestation] def networkPrefixValidation(bytes: Array[Byte])(implicit
    networkPrefix: NetworkPrefix
  ): ValidatedNec[AddressValidationError, NetworkPrefix] =
    bytes.headOption
      .toValidNec(AddressValidationErrors.InvalidAddress)
      .andThen(prefix =>
        NetworkType
          .pickNetworkType(prefix)
          .toValidNec(AddressValidationErrors.InvalidNetworkPrefix)
          .map(_.netPrefix)
          .combine(Validated.condNec(prefix == networkPrefix, prefix, AddressValidationErrors.NetworkTypeMismatch))
      )

  private[attestation] def lengthValidation(bytes: Array[Byte]): ValidatedNec[AddressValidationError, Array[Byte]] =
    Validated.condNec(
      bytes.length == AttestationBytesOps.EncodedAddressLength,
      bytes,
      AddressValidationErrors.InvalidAddressLength
    )

  private[attestation] def checksumValidation(
    bytes: Array[Byte]
  ): ValidatedNec[AddressValidationError, Array[Byte]] =
    Validated.condNec(
      Address.checksum(bytes.dropRight(Address.ChecksumLength)) sameElements bytes.takeRight(
        Address.ChecksumLength
      ),
      bytes,
      AddressValidationErrors.InvalidChecksum
    )

  private[attestation] def parseValidation(bytes: Array[Byte]): ValidatedNec[AddressValidationError, Address] =
    bytes.decodePersisted[Address].leftMap(AddressValidationErrors.AddressDecodeFailure).toValidatedNec
}

object AttestationBytesOps {

  // encoded addresses are 38 bytes (1 for network prefix, 1 for type prefix, 32 for content, 4 for checksum)
  // ENCODED ADDRESS != ADDRESS (Address are contained in an encoded address)
  private val EncodedAddressLength: Int = Address.addressSize + Address.ChecksumLength

  implicit private val prefixSemigroup: Semigroup[NetworkPrefix] = (_, b) => b
  implicit private val byteArraySemigroup: Semigroup[Array[Byte]] = (_, b) => b

  trait ToAttestationBytesOps {

    implicit def attestationBytesOpsFromBytes(bytes: Array[Byte]): AttestationBytesOps =
      new AttestationBytesOps(bytes)

    implicit def attestationBytesOpsFromBase58(data: Base58Data): AttestationBytesOps =
      new AttestationBytesOps(data.value)

    implicit def attestationBytesOpsFromBase16(data: Base16Data): AttestationBytesOps =
      new AttestationBytesOps(data.value)

  }
}
