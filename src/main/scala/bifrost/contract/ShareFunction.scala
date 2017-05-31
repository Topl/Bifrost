package bifrost.contract

import breeze.interpolation.LinearInterpolator
import breeze.linalg.{*, DenseMatrix, DenseVector}

/**
  * Share function dictates how over delivered goods' value are split up
  * First double is going to be the x-axis number.
  * @param points: Sequence of
  *               Double: Yield. ex. 1.25 = 25% over delivery
  *               Double: Producer's cut in percentage
  *               Double: Hub's cut in percentage
  *               Double: Investor's cut in percentage
  */
abstract class ShareFunction(val points: Seq[(Double, (Double, Double, Double))]) {

  // delivered values must be distinct
  require(points.map(_._1).distinct.size == points.size)
  points.map(_._2).foreach(t => {
    require(t._1 + t._2 + t._3 - 1 < 10E-5)
    require(t._1 > 0); require(t._2 > 0); require(t._3 > 0)
  })

  val functionType = "ShareFunction"
  def evaluate(x: Double): (Double, Double, Double)
}

/**
  * This will try to interpolate between points
  * @param points: Sequence of
  *               Double: Yield. ex. 1.25 = 25% over delivery
  *               Double: Producer's cut in percentage
  *               Double: Hub's cut in percentage
  *               Double: Investor's cut in percentage
  */
class PiecewiseLinearMultiple(points: Seq[(Double, (Double, Double, Double))]) extends ShareFunction(points) {

  override val functionType = "PiecewiseLinearMultiple"

  val inputs: DenseVector[Double] = DenseVector[Double](points.map(_._1):_*)
  val outputs: DenseMatrix[Double] = DenseMatrix(points.map(p => DenseVector(p._2._1, p._2._2, p._2._3)):_*).t
  val f: DenseVector[LinearInterpolator[Double]] = outputs(*,::).map(row => LinearInterpolator[Double](inputs, row))

  def evaluate(yieldPercentage: Double): (Double, Double, Double) = {
    val res = f.map(_(yieldPercentage.toDouble))
    (res(0), res(1), res(2))
  }
}