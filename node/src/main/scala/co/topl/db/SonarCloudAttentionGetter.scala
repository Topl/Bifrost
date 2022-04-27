package co.topl.db

/**
 * This object is totally unneeded by bifrost. I am adding it with some things that Sonar should complain about so that
 * I can see that the integration with Sonarcloud is correctly set up.
 *
 * This should be deleted as soon as the Sonarcloud integration is working.
 */
object sonarCloudAttentionGetter {

  def DoIt(x: Int): Unit = {
    if (!(x != 4))
      println(x)
    val a = 4
    val b = 4
    if (a == a)
      println(a - a)
  }

  // TODO blah more

  // FIXME blah

  def Dont() = {}
}
