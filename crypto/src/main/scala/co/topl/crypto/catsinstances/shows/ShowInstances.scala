package co.topl.crypto.catsinstances.shows

import cats.Show
import co.topl.crypto.hash.digest.{IncorrectSize, InvalidDigestFailure}

trait ShowInstances {

  implicit val invalidDigestFailureShow: Show[InvalidDigestFailure] = { case IncorrectSize =>
    s"Digest is the incorrect size"
  }
}
