package org.geolatte.panoramaserver


import scala.math._

/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/23/12
 */
package object Types {

  /**
   * A type for fractional Pixels
   *
   */
  sealed case class FractPixel(row: Float, col: Float);

  /**
   * a Pixel refers to a single raster cell by row (first component) and column (second component).
   *
   * <p>Spatially, {@code Pixels} correspond to the centers of the raster cells </p>
   *
   */
  sealed case class Pixel(row: Int, col: Int)

  /**
   * A viewing angle
   * @param h the horizontal angle (will be normalized to within in the range 0 - 2*Pi)
   * @param v the vertical angle (will be normalized to within in the range 0 - Pi)
   */
  sealed case class ViewingAngle(private var h: Double, private var v: Double) {
    private def wrapHorizontal(v: Double) = {
      val rem = v % Pi
      if (rem < 0) Pi + rem
      else -Pi + rem
    }
    private def wrapVertical(v: Double)= {
      val rem = v % Pi
      if (rem < 0) Pi  + rem
      else rem
    }
    val horizontal = if (h < -Pi || h > Pi)  wrapHorizontal(h) else h

    val vertical = if (v < 0 || v > Pi) wrapVertical(v) else v
  }

  object ViewingAngle {
    implicit def pair2ViewingAngle(pair: (Double, Double)): ViewingAngle = new ViewingAngle(pair._1, pair._2)
  }

  object Pixel {
    implicit def pair2Pixel(pair: (Int, Int)): Pixel = new Pixel(pair._1, pair._2)
  }

  object FractPixel {
    implicit def pairFloat2Pixel(pair: (Float, Float)): FractPixel = new FractPixel(pair._1, pair._2)

    implicit def pairDouble2Pixel(pair: (Double, Double)): FractPixel = new FractPixel(pair._1.toFloat, pair._2.toFloat)
  }


}



