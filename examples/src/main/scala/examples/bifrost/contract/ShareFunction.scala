package examples.bifrost.contract

import breeze.interpolation.LinearInterpolator
import breeze.linalg.{*, DenseMatrix, DenseVector}

abstract class ShareFunction(val points: Seq[(Double, (Double, Double, Double))]) {

  // delivered values must be distinct
  require(points.map(_._1).distinct.size == points.size)

  val functionType = "ShareFunction"
  def evaluate(x: Double): (Double, Double, Double)
}

/**
  * This will try to interpolate between points
  */
class PiecewiseLinearMultiple(points: Seq[(Double, (Double, Double, Double))]) extends ShareFunction(points) {

  override val functionType = "PiecewiseLinearMultiple"

  val inputs: DenseVector[Double] = DenseVector[Double](points.map(_._1):_*)
  val outputs: DenseMatrix[Double] = DenseMatrix(points.map(p => DenseVector(p._2._1, p._2._2, p._2._3)):_*).t
  val f: DenseVector[LinearInterpolator[Double]] = outputs(*,::).map(row => LinearInterpolator[Double](inputs, row))

  def evaluate(timestamp: Double): (Double, Double, Double) = {
    val res = f.map(_(timestamp.toDouble))
    (res(0), res(1), res(2))
  }
}