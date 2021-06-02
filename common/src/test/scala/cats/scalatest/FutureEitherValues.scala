package cats.scalatest

import cats.data.EitherT
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Future
import scala.language.implicitConversions

trait FutureEitherValues extends EitherValues with ScalaFutures {

  implicit class eitherTOps[L, R](eitherT: EitherT[Future, L, R]) {

    def futureRightValue: R =
      eitherT.value.futureValue.value

    def futureLeftValue: L =
      eitherT.value.futureValue.leftValue
  }
}
