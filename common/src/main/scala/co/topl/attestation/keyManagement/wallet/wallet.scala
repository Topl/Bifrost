package co.topl.attestation.keyManagement

import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import org.bouncycastle.crypto.digests.SHA512Digest
import scodec.bits.ByteVector

import scala.language.implicitConversions

// https://raw.githubusercontent.com/input-output-hk/adrestia/master/user-guide/static/Ed25519_BIP.pdf
package object wallet {

  private[wallet] def hmac512(data: ByteVector): ByteVector = {
    val mac = new HMac(new SHA512Digest())
    mac.update(data.toArray, 0, data.length.toInt)
    val out = new Array[Byte](64)
    mac.doFinal(out, 0)
    ByteVector.view(out)
  }

  private[wallet] def hmac512WithKey(key: ByteVector, data: ByteVector): ByteVector = {
    val mac = new HMac(new SHA512Digest())
    mac.init(new KeyParameter(key.toArray))
    mac.update(data.toArray, 0, data.length.toInt)
    val out = new Array[Byte](64)
    mac.doFinal(out, 0)
    ByteVector.view(out)
  }

  object implicits extends WalletCodec.AsBytesInstances
}
