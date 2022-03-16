package co.topl.consensus

import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._
import co.topl.consensus.ThresholdLentzMethod._

class LentzSpec extends AnyFlatSpec
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with MockFactory
  with EitherValues{
  behavior of "Lentz Threshold Calculation"
  it should "calculate to desired precision" in {
    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()
      val outTime = (t1 - t0)*1.0e-9
      val tString = "%6.6f".format(outTime)
      println("Elapsed time: " + tString + " s")
      result
    }
    val maxIter = 10000
    def regression(prec:Int,stake:BigInt,netStake:BigInt, f:Ratio) = {
      val precision = prec
      val relativeStake = Ratio(stake,netStake)
      val coefficient = Ratio(math.log(1.0-f.toDouble),5)
      val ref = BigDecimal(1 - math.pow(1.0 - f.toDouble, relativeStake.toDouble))
      val res = Ratio(1) - exp(coefficient * relativeStake, maxIter, precision)._1
      val absError = (res.toBigDecimal - ref).abs
      (stake,netStake,res,res.toBigDecimal,ref,absError < math.pow(10,-precision))
    }

    val precision = 38
    val f = Ratio(500,1000)
    val relativeStake = Ratio(BigInt(10).pow(precision)/2,BigInt(10).pow(precision))
    val coefficient = Ratio(math.log(1.0-f.toDouble),precision)
    val ref = 1 - math.pow(1.0 - f.toDouble, relativeStake.toDouble)
    val res = 1 - exp(coefficient * relativeStake, maxIter, precision)._1.toBigDecimal
    println("e: "+exp(Ratio(1),10000,38))
    println("approx e: "+ratioinal_approximation(exp(Ratio(1),10000,38)._1,100000,10000))
    println("log(1/2): "+log1p(Ratio(-1,2),10000,38))
    println("Res:" + res)
    println("Ref:" + ref)
    val absError = (res - ref).abs
    val relError = (res/ref - 1.0).abs
    println("Absolute Error:" + absError)
    println("Relative Error:" + relError)
    println(res)
    val res2 = log1p(Ratio(-9,10), maxIter, precision)._1
    println(ratioinal_approximation(res2,BigInt(10).pow(precision),1000).toBigDecimal)
    println(res2.toBigDecimal)
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
    println("Relative Stake: Threshold Bytes, Threshold, Threshold Value, Reference Value")
    data1.foreach(p => println(p._1+"/10^"+math.log10(p._2.toDouble).toInt+ ": " +(p._3.numerator.toByteArray.length+p._3.denominator.toByteArray.length)+", "+p._3+", "+p._4+", "+p._5))
    data2.foreach(p => println(p._1+"/10^"+math.log10(p._2.toDouble).toInt+ ": " +(p._3.numerator.toByteArray.length+p._3.denominator.toByteArray.length)+", "+ p._3+", "+p._4+", "+p._5))
    def isSorted(s: Seq[Ratio]): Boolean = s match {
      case Seq() => true
      case Seq(_) => true
      case _ => s.sliding(2).forall { case Seq(x, y) =>
        val out = x < y
        if (!out) {
          println(x.toBigDecimal.toString())
          println(y.toBigDecimal.toString())
        }
        out
      }
    }

    isSorted(data1.map(p => p._3)) shouldBe true
    isSorted(data2.map(p => p._3)) shouldBe true
    absError < BigDecimal(10).pow(-math.min(precision,15)) shouldBe true

  }
}
