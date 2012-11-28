package org.geolatte.panoramaserver


import scala.math._

/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/23/12
 */
object Types {

  /**
   * Dimension (width and height) of an Image with top-left coordinates origin
   */
  case class Dimension(origin: Pixel, width: Int, height: Int) {
    def center: FractPixel = FractPixel((height - 1) / 2, (width - 1) / 2)
  }

  trait Coordinate {
    type T <: Coordinate

    val x: Double
    val y: Double

    def -(other: T): T = make(this.x - other.x, this.y - other.y)

    def +(other: T): T = make(this.x + other.x, this.y + other.y)

    def *(scale: Double): T = make(x * scale, y * scale)

    def /(scale: Double): T = make(x / scale, y / scale)

    def make(row: Double, col: Double): T


  };

  /**
   * A type for fractional Pixels
   *
   */
  case class FractPixel(x: Double, y: Double) extends Coordinate {
    type T = FractPixel
    def toPixel: Pixel = Pixel(round(x).toInt, round(y).toInt)
    def make(x: Double, y: Double): FractPixel = FractPixel(x, y)
  }

  /**
   * a Pixel refers to a single raster cell by row (first component) and column (second component).
   *
   * <p>Spatially, {@code Pixels} correspond to the centers of the raster cells </p>
   *
   */
  sealed case class Pixel(x: Int, y: Int) {
//    def toFractPixel: FractPixel = FractPixel(x, y)
  }

  /**
   * A viewing angle
   * @param h the horizontal angle (will be normalized to within in the range [-Pi, Pi]) or longitude
   * @param v the vertical angle (will be normalized to within in the range [-Pi/2 , Pi/2]) or latitude
   */
  case class ViewingAngle(private val h: Double, private val v: Double) extends Coordinate {
    type T = ViewingAngle

    private def wrapHorizontal(v: Double) = {
      val rem = v % Pi
      if (rem < 0) Pi + rem
      else -Pi + rem
    }

    private def wrapVertical(v: Double) = {
      val rem = v % (Pi/2)
      if (rem < 0) (Pi/2) + rem
      else -Pi/2 + rem
    }

    val x = if (h < -Pi || h >= Pi) wrapHorizontal(h) else h
    val y = if (v < -Pi/2 || v >= Pi/2) wrapVertical(v) else v

    val lon = x //longitude synonym for x
    val lat = y //latitude synonym for y

    override def make(x: Double, y: Double) = ViewingAngle(x, y)

  }

  object ViewingAngle {
    implicit def pair2ViewingAngle(pair: (Double, Double)): ViewingAngle = new ViewingAngle(pair._1, pair._2)
  }

  object Pixel {
    implicit def pair2Pixel(pair: (Int, Int)): Pixel = Pixel(pair._1, pair._2)

    implicit def pixel2FractPixel(p: Pixel): FractPixel = FractPixel(p.x, p.y)
  }

  object FractPixel {
    implicit def pairDouble2Pixel(pair: (Double, Double)): FractPixel = new FractPixel(pair._1, pair._2)
  }


}



