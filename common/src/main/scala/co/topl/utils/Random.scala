package co.topl.utils

import java.security.SecureRandom

object Random {
  def randomBytes(length: Int = 32): Array[Byte] = {
    val r = new Array[Byte](length)
    new SecureRandom().nextBytes(r) //overrides r
    r
  }
}
