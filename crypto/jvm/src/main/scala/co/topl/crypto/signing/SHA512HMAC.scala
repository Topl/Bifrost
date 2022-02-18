package co.topl.crypto.signing

import co.topl.models.Bytes
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter

object SHA512HMAC {

  def hmac512WithKey(key: Array[Byte], data: Array[Byte]): Bytes = {
    val mac = new HMac(new SHA512Digest())
    mac.init(new KeyParameter(key))
    mac.update(data, 0, data.length)
    val out = new Array[Byte](64)
    mac.doFinal(out, 0)
    Bytes(out)
  }
}
