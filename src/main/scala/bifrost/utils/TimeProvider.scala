package bifrost.utils

object TimeProvider {
  type Time = Long
}

trait TimeProvider {
  def time(): TimeProvider.Time
}
