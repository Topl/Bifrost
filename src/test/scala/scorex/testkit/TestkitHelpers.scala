package scorex.testkit

trait TestkitHelpers {

  val MinTestsOk = 100
  def check(f: Int => Unit): Unit = (0 until 100) foreach (i => f(i))

  /**
    * @param block - function to profile
    * @return - execution time in seconds and function result
    */
  def profile[R](block: => R): (Float, R) = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    ((t1 - t0).toFloat / 1000000000, result)
  }
}
