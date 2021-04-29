package co.topl.utils

object LocalTimeProvider extends TimeProvider {

  override def time: TimeProvider.Time =
    System.currentTimeMillis()
}
