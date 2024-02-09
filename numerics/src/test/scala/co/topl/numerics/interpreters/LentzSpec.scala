package co.topl.numerics.interpreters

import cats.implicits._
import cats.effect.IO
import co.topl.models.utility.Ratio
import co.topl.numerics.implicits._
import munit._

class LentzSpec extends CatsEffectSuite {

  type F[A] = IO[A]
  val maxIter = 10000
  val precision = 40

  test("calculate to desired precision") {
    val f = Ratio(500, 1000)
    val relativeStake = Ratio(BigInt(10).pow(precision) / 2, BigInt(10).pow(precision))
    val coefficient = Ratio(math.log(1.0 - f.toDouble), precision)
    val ref = 1 - math.pow(1.0 - f.toDouble, relativeStake.toDouble)

    for {
      exp <- ExpInterpreter.make[F](maxIter, precision)
      res <- exp.evaluate(coefficient * relativeStake).map(_.toBigDecimal).map(1 - _)
      absError = (res - ref).abs
      data1 = for {
        n <- Range(0, 100)
      } yield regression(precision, BigInt(n), BigInt(10).pow(precision), f)
      data2 = for {
        n <- Range(0, 100)
      } yield regression(precision, BigInt(10).pow(precision) / 2 - 100 + BigInt(n), BigInt(10).pow(precision), f)

      _ <- (
        isSorted(data1.map(p => p._3)) &&
        isSorted(data2.map(p => p._3)) &&
        absError < BigDecimal(10).pow(-math.min(precision, 15))
      ).pure[F].assert
    } yield ()
  }

  def regression(prec: Int, stake: BigInt, netStake: BigInt, f: Ratio) = {
    val precision = prec
    val exp = ExpInterpreter.make[F](maxIter, precision).unsafeRunSync()
    val relativeStake = Ratio(stake, netStake)
    val coefficient = Ratio(math.log(1.0 - f.toDouble), 5)
    val ref = BigDecimal(1 - math.pow(1.0 - f.toDouble, relativeStake.toDouble))
    val res = Ratio.One - exp.evaluate(coefficient * relativeStake).unsafeRunSync()
    val absError = (res.toBigDecimal - ref).abs
    (stake, netStake, res, res.toBigDecimal, ref, absError < math.pow(10, -precision))
  }

  def isSorted(s: Seq[Ratio]): Boolean = s match {
    case Seq()  => true
    case Seq(_) => true
    case _      => s.sliding(2).forall { case Seq(x, y) => x < y }
  }
}
