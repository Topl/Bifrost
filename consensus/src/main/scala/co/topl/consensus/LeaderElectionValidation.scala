package co.topl.consensus

import cats.Monad
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.codecs.bytes.typeclasses.Signable
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import scalacache.caffeine.CaffeineCache
import scala.collection.concurrent.TrieMap

object LeaderElectionValidation {

  case class VrfConfig(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)

  case class VrfArgument(eta: Eta, slot: Slot)

  implicit val signableVrfArgument: Signable[VrfArgument] =
    arg => arg.eta.data ++ Bytes(BigInt(arg.slot).toByteArray)

  object Eval {

    def make[F[_]: Monad](
      config:             VrfConfig,
      blake2b512Resource: UnsafeResource[F, Blake2b512]
    ): LeaderElectionValidationAlgebra[F] =
      new LeaderElectionValidationAlgebra[F] {
        def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio] = {
          val maxIter = 10000
          val prec = 10
          val difficultyCurve = MathUtils.difficultyCurve(slotDiff,config)
          if (difficultyCurve == Ratio(1)) {
            Ratio(1).pure[F]
          } else {
            val coefficient = MathUtils.coefficientCache.get(
              MathUtils.CoefficientJob(math.min(slotDiff,config.lddCutoff+1),maxIter,prec,config)
            )
            val result = MathUtils.thresholdCache.get(
              MathUtils.ThresholdJob(coefficient*relativeStake,maxIter,prec)
            )
            result.pure[F]
          }
        }

        /**
         * Determines if the given proof meets the threshold to be elected slot leader
         * @param threshold the threshold to reach
         * @param proof the proof output
         * @return true if elected slot leader and false otherwise
         */
        def isSlotLeaderForThreshold(threshold: Ratio)(rho: Rho): F[Boolean] = blake2b512Resource.use {
          implicit blake2b512 =>
            val testRhoHash = Ed25519VRF.rhoToRhoTestHash(rho)
            val testRhoHashBytes = testRhoHash.sizedBytes.data
            // TODO: Where does this come from?
            (threshold > testRhoHashBytes.toIterable
              .zip(1 to testRhoHashBytes.length.toInt) // zip with indexes starting from 1
              .foldLeft(Ratio(0)) { case (net, (byte, i)) =>
                net + Ratio(BigInt(byte & 0xff), BigInt(2).pow(8 * i))
              })
              .pure[F]
        }
      }
  }
}

private object MathUtils {

  case class ThresholdJob(arg:Ratio, maxIter:Int, precision:Int)
  case class CoefficientJob(slotInterval:Slot,maxIter:Int,precision:Int,config:VrfConfig)

  val cacheSize = 1000
  //TODO replace with caffeine cache
  val thresholdCache: LoadingCache[ThresholdJob, Ratio] = CacheBuilder.newBuilder()
    .maximumSize(cacheSize)
    .build[ThresholdJob,Ratio](
      new CacheLoader[ThresholdJob,Ratio] {
        def load(id:ThresholdJob):Ratio = {
          Ratio(1) - ThresholdLentzMethod.ratioinal_approximation(
            ThresholdLentzMethod.exp(id.arg,id.maxIter,id.precision)._1,id.precision,id.maxIter
          )
        }
      }
    )

  val coefficientCache: LoadingCache[CoefficientJob, Ratio] = CacheBuilder.newBuilder()
    .maximumSize(cacheSize)
    .build[CoefficientJob,Ratio](
      new CacheLoader[CoefficientJob,Ratio] {
        def load(id:CoefficientJob):Ratio = {
          if (id.slotInterval>id.config.lddCutoff) {
            ThresholdLentzMethod.ratioinal_approximation(
              ThresholdLentzMethod.log1p(
                Ratio(-1)*id.config.baselineDifficulty,
                id.maxIter,id.precision)._1,
              id.precision,id.maxIter
            )
          } else {
            ThresholdLentzMethod.ratioinal_approximation(
              ThresholdLentzMethod.log1p(
                Ratio(-1)*difficultyCurve(
                  id.slotInterval,
                  id.config),
                id.maxIter,id.precision)._1,
              id.precision,id.maxIter
            )
          }
        }
      }
    )

  // Local Dynamic Difficulty curve
  def difficultyCurve(slotDiff:Long, cfg: VrfConfig): Ratio = if (slotDiff>cfg.lddCutoff) cfg.baselineDifficulty
  else Ratio(BigInt(slotDiff), BigInt(cfg.lddCutoff)) * cfg.amplitude

}

private object ThresholdLentzMethod {

  // To preserve performance properties the following imperative numerical code must not be refactored

  /**
   * Implmentation of Farey approximation technique following this blog post
   * https://www.johndcook.com/blog/2010/10/20/best-rational-approximation/
   * This method reduces the byte size of ratios by finding an
   * approximate answer with a smaller numerator and denominator.
   * Returns a ratio with a denominator no greater than maxDenominator that approximates the input ratio.
   * The approximation gets better with number of iterations.
   * I've modified the algorithm to work with numbers greater than 1 and all negative numbers
   * @param input ratio to be reduced in size
   * @param maxDenominator maximum resultant denominator
   * @param maxIter maximum number of iterations
   * @return a new ratio with smaller integer values that approximates the input
   */
  def ratioinal_approximation(input:Ratio, maxDenominator:BigInt, maxIter:Int):Ratio = {
    val x = if (input.denominator<0) Ratio(-input.numerator,-input.denominator)
    else input
    var output = input
    val q:BigInt = x.numerator/x.denominator
    val r:BigInt = x.numerator%x.denominator
    val sign = if (x.numerator > 0 && x.denominator > 0) {
      1
    } else {
      -1
    }
    val absx = Ratio(r,x.denominator).abs
    var a = BigInt(0)
    var b = BigInt(1)
    var c = BigInt(1)
    var d = BigInt(1)
    var j = 0
    while (b<=maxDenominator && d <= maxDenominator && j<=maxIter) {
      val med = Ratio(a+c,b+d)
      if (absx == med) {
        if (b+d <=maxDenominator) {
          output = Ratio(sign*(a+c),b+d)
        } else if (d>b) {
          output = Ratio(sign*c,d)
        } else {
          output = Ratio(sign*a,b)
        }
      } else if (absx > med) {
        a = a+c
        b = b+d
      } else {
        c = a+c
        d = b+d
      }
      j = j+1
    }
    if (b>maxDenominator) {
      output = Ratio(sign*c,d)
    } else {
      output = Ratio(sign*a,b)
    }
    output + Ratio(q)
  }

  /**
   * Implementation of modified Lentz's method from "Numerical Recipes in Fortran 77" Second Edition Section 5.2
   * William H. Press, Saul A. Teukolsky, William T. Vetterling, and Brian P. Flannery. 1992.
   * Numerical recipes in FORTRAN (2nd ed.): the art of scientific computing. Cambridge University Press, USA.
   *
   * The numerical technique uses a set of coefficients to calculate a nested fraction iteratively,
   * avoiding nested recursion and providing a much more performant algorithm
   * @param maxIter maximum number of iterations
   * @param prec desired precision
   * @param a a coefficients that map integers to ratios
   * @param b b coefficients that map integers to ratios
   * @param limit limit the size of the numerator and denominator
   * @param limitBytes maximum size of the byte length of the resultant ratio
   * @return a tuple containing:
   *         ratio approximating the nested fraction,
   *         false if method converged true otherwise,
   *         number of iterations
   */
  def modified_lentz_method(
                             maxIter:Int,
                             prec:Int,
                             a: Int => Ratio,
                             b:Int => Ratio,
                             limit:Boolean = false,
                             limitBytes:Int = 2048
                           ): (Ratio,Boolean,Int) = {
    val bigFactor = BigInt(10).pow(prec+10)
    val tinyFactor = Ratio(1,bigFactor)
    val truncationError:Ratio = Ratio(1,BigInt(10).pow(prec+1))
    var fj:Ratio = {
      if (b(0)==Ratio(0)) tinyFactor
      else b(0)
    }
    var cj:Ratio = fj
    var dj:Ratio = Ratio(0)
    var deltaj = Ratio(1)
    var diff = Ratio(1)
    var error:Boolean = true
    def loop(j:Int):Unit = {
      dj = b(j) + a(j) * dj
      if (dj == Ratio(0)) dj = tinyFactor
      cj = b(j) + a(j)/cj
      if (cj == Ratio(0)) cj = tinyFactor
      dj = Ratio(dj.denominator,dj.numerator)
      deltaj = cj*dj
      val fjm1 = fj
      fj = fj*deltaj
      diff = fj - fjm1
      error = j match {
        case _ if j > 1 => (deltaj-Ratio(1)).abs > truncationError
        case _ => true
      }
      if (limit) {
        if (fj.numerator.toByteArray.length + fj.denominator.toByteArray.length > limitBytes
          || cj.numerator.toByteArray.length + cj.denominator.toByteArray.length > limitBytes
          || dj.numerator.toByteArray.length + dj.denominator.toByteArray.length > limitBytes
        ) {
          fj = ratioinal_approximation(fj,bigFactor,1000)
          dj = ratioinal_approximation(dj,bigFactor,1000)
          cj = ratioinal_approximation(cj,bigFactor,1000)
        }
      }
    }
    var j = 1
    while (j < maxIter+1 && error) {
      loop(j)
      j = j+1
    }
    if (fj.denominator<0) fj = Ratio(-fj.numerator,-fj.denominator)
    (fj,error,j)
  }

  /**
   * Returns the exponent base natural number of the argument
   * @param x argument
   * @param maxIter maximum number of iterations
   * @param prec desired precision
   * @return exp(x)
   */

  def exp(x:Ratio, maxIter:Int, prec:Int):(Ratio,Boolean,Int) = {
    def a(j:Int):Ratio = j match {
      case 0 => Ratio(0)
      case 1 => Ratio(1)
      case 2 => Ratio(-1)*x
      case _ => Ratio(-j+2)*x
    }
    def b(j:Int):Ratio = j match {
      case 0 => Ratio(0)
      case 1 => Ratio(1)
      case _ => Ratio(j-1)+x
    }
    if (x == Ratio(0)) (Ratio(1),true,0)
    else modified_lentz_method(maxIter,prec,a,b)
  }

  /**
   * Returns the natural logarithm of the argument plus one
   * @param x argument
   * @param maxIter maximum number of iterations
   * @param prec desired precision
   * @return log_e(1+x)
   */
  def log1p(x:Ratio, maxIter:Int, prec:Int):(Ratio,Boolean,Int) = {
    def a(j:Int):Ratio = j match {
      case 0 => Ratio(0)
      case 1 => x
      case _ => Ratio(j-1)*Ratio(j-1)*x
    }
    def b(j:Int):Ratio = j match {
      case 0 => Ratio(0)
      case 1 => Ratio(1)
      case _ => Ratio(j) - Ratio(j-1)*x
    }
    modified_lentz_method(maxIter,prec,a,b)
  }

}