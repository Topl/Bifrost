package co.topl.numerics

import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._

trait LentzMethod {

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
   * @return a tuple containing:
   *         ratio approximating the nested fraction,
   *         false if method converged true otherwise,
   *         number of iterations
   */
  private[numerics] def modified_lentz_method(
                                               maxIter:Int,
                                               prec:Int,
                                               a: Int => Ratio,
                                               b:Int => Ratio
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
    }
    var j = 1
    while (j < maxIter+1 && error) {
      loop(j)
      j = j+1
    }
    if (fj.denominator<0) fj = Ratio(-fj.numerator,-fj.denominator)
    (fj,error,j)
  }

}
