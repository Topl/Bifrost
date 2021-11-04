package co.topl.utils.catsInstances.shows

import cats.Show
import co.topl.attestation.Address
import co.topl.crypto.hash.digest.Digest
import co.topl.utils.codecs.binary._

trait ShowInstances {

  implicit val showBytes: Show[Array[Byte]] = fromBase58Show

  implicit val addressShow: Show[Address] = fromBase58Show

  implicit def digestShow[T: Digest]: Show[T] = fromBase58Show
}
