package co.topl.consensus

import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.models.utility.Ratio
import co.topl.numerics._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class LentzSpec extends AnyFlatSpec
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with MockFactory
  with EitherValues{
  behavior of "Lentz Threshold Calculation"

  type F[A] = IO[A]
  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]

  it should "calculate to desired precision" in {
    val maxIter = 10000
    val precision = 38
    val exp = ExpInterpreter.make[F](maxIter,precision).unsafeRunSync()
    val rationalApprox = RationalApproximationInterpreter.make[F](100000,maxIter).unsafeRunSync()
    val log1p = Log1pInterpreter.make[F](maxIter,precision).unsafeRunSync()
    def printUnsafe(any:Any):Unit = {
      Logger[F].debug(any.toString).unsafeRunSync()
    }

    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()
      val outTime = (t1 - t0)*1.0e-9
      val tString = "%6.6f".format(outTime)
      printUnsafe("Elapsed time: " + tString + " s")
      result
    }
    def regression(prec:Int,stake:BigInt,netStake:BigInt, f:Ratio) = {
      val precision = prec
      val exp = ExpInterpreter.make[F](maxIter,precision).unsafeRunSync()
      val relativeStake = Ratio(stake,netStake)
      val coefficient = Ratio(math.log(1.0-f.toDouble),5)
      val ref = BigDecimal(1 - math.pow(1.0 - f.toDouble, relativeStake.toDouble))
      val res = Ratio(1) - exp.evaluate(coefficient * relativeStake).unsafeRunSync()
      val absError = (res.toBigDecimal - ref).abs
      (stake,netStake,res,res.toBigDecimal,ref,absError < math.pow(10,-precision))
    }

    val f = Ratio(500,1000)
    val relativeStake = Ratio(BigInt(10).pow(precision)/2,BigInt(10).pow(precision))
    val coefficient = Ratio(math.log(1.0-f.toDouble),precision)
    val ref = 1 - math.pow(1.0 - f.toDouble, relativeStake.toDouble)
    val res = 1 - exp.evaluate(coefficient * relativeStake).unsafeRunSync().toBigDecimal
    printUnsafe("e: "+exp.evaluate(Ratio(1)).unsafeRunSync())
    printUnsafe("approx e: "+rationalApprox.rationalApproximation(exp.evaluate(Ratio(1)).unsafeRunSync()).unsafeRunSync())
    printUnsafe("log(1/2): "+log1p.evaluate(Ratio(-1,2)).unsafeRunSync())
    printUnsafe("Res:" + res)
    printUnsafe("Ref:" + ref)
    val absError = (res - ref).abs
    val relError = (res/ref - 1.0).abs
    printUnsafe("Absolute Error:" + absError)
    printUnsafe("Relative Error:" + relError)
    printUnsafe(res)
    val res2 = log1p.evaluate(Ratio(-9,10)).unsafeRunSync()
    printUnsafe(rationalApprox.rationalApproximation(res2).unsafeRunSync().toBigDecimal)
    printUnsafe(res2.toBigDecimal)
    val data1 = time(for {
      n <- Range(0,100)
    } yield {
      regression(precision,BigInt(n),BigInt(10).pow(precision),f)
    })
    val data2 = time(for {
      n <- Range(0,100)
    } yield {
      regression(precision,BigInt(10).pow(precision)/2-100+BigInt(n),BigInt(10).pow(precision),f)
    })
    printUnsafe("Relative Stake: Threshold Bytes, Threshold, Threshold Value, Reference Value")
    data1.foreach(p => printUnsafe(p._1+"/10^"+math.log10(p._2.toDouble).toInt+ ": " +(p._3.numerator.toByteArray.length+p._3.denominator.toByteArray.length)+", "+p._3+", "+p._4+", "+p._5))
    data2.foreach(p => printUnsafe(p._1+"/10^"+math.log10(p._2.toDouble).toInt+ ": " +(p._3.numerator.toByteArray.length+p._3.denominator.toByteArray.length)+", "+ p._3+", "+p._4+", "+p._5))
    def isSorted(s: Seq[Ratio]): Boolean = s match {
      case Seq() => true
      case Seq(_) => true
      case _ => s.sliding(2).forall { case Seq(x, y) =>
        val out = x < y
        if (!out) {
          printUnsafe(x.toBigDecimal.toString())
          printUnsafe(y.toBigDecimal.toString())
        }
        out
      }
    }

    isSorted(data1.map(p => p._3)) shouldBe true
    isSorted(data2.map(p => p._3)) shouldBe true
    absError < BigDecimal(10).pow(-math.min(precision,15)) shouldBe true

  }
}
