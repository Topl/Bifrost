package co.topl.loadtesting

import akka.NotUsed
import akka.stream.IOResult
import akka.stream.scaladsl.{FileIO, Flow, Sink}
import akka.util.ByteString
import simulacrum.typeclass

import java.nio.file.{Paths, StandardOpenOption}
import scala.language.implicitConversions
import scala.concurrent.Future

package object statistics {

  /**
   * A type-class which can convert a value into a CSV friendly log.
   * @tparam T the type of value to converts
   */
  @typeclass
  trait ToStatisticsCsvLog[T] {

    /**
     * Converts the value to a CSV friendly log message.
     * @param t the value to convert into CSV
     * @return the CSV log message
     */
    def toCsvLog(t: T): String
  }

  /**
   * Sink which outputs the given statistics to a statistics file.
   */
  object StatisticsSink {

    import ToStatisticsCsvLog.ops._

    /**
     * Instantiates a new statistics sink which outputs input statistics to the given output file.
     * If the output file doesn't exist, it will be created.
     * @param path the path to output logs to
     * @tparam T the type of value to generate a CSV friendly log from
     * @return `NotUsed` signalling completion
     */
    def apply[T: ToStatisticsCsvLog](path: String): Sink[T, Future[IOResult]] =
          FileIO.toPath(Paths.get(path), options = Set(StandardOpenOption.CREATE, StandardOpenOption.APPEND))
            .contramap[T](t => ByteString(t.toCsvLog + "\n"))
  }
}
