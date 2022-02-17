package co.topl.loadtesting

import akka.stream.IOResult
import akka.stream.scaladsl.{FileIO, Sink}
import akka.util.ByteString

import java.nio.file.{Paths, StandardOpenOption}
import scala.concurrent.Future
import scala.language.implicitConversions

package object statistics {

  /**
   * Instantiates a new statistics sink which outputs input statistics to the given output file.
   * If the output file doesn't exist, it will be created.
   * @param path the path to output logs to
   * @tparam T the type of value to generate a CSV friendly log from
   * @return a sink which accepts a value of `T` and returns a `Future[IOResult]` signaling write success or failure
   */
  def toCsvSink[T: ToStatisticsCsvLog](path: String): Sink[T, Future[IOResult]] =
    FileIO
      .toPath(Paths.get(path), options = Set(StandardOpenOption.CREATE, StandardOpenOption.APPEND))
      .contramap[T](t => ByteString(ToStatisticsCsvLog[T].toCsvLog(t) + "\n"))

}
