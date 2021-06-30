package co.topl.attestation.keyManagement.wallet

import co.topl.utils.SizedByteVector.implicits._
import co.topl.utils.codecs.{AsBytes, Infallible}

object WalletCodec {
  trait AsBytesInstances {
    implicit val privateKeyAsBytes: AsBytes[Infallible, ExtendedPrivateKey] =
      AsBytes.infallible(p => p.leftKey.toArray ++ p.rightKey.toArray)

    implicit val publicKeyBytes: AsBytes[Infallible, ExtendedPublicKey] = AsBytes.infallible(p => p.bytes.toArray)
  }
}
