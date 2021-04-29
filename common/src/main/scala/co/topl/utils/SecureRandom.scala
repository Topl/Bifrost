package co.topl.utils

object SecureRandom {

  def randomBytes(length: Int = 32): Array[Byte] = {
    val r = new Array[Byte](length)
    new java.security.SecureRandom().nextBytes(r) //overrides r
    r
  }
}
