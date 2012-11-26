package org.geolatte.panoramaserver


import scala.math._

/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/23/12
 */
object Types {

  /**
   * Dimension (width and height) of an Image with top-left coordinates: (0,0)
   */
  case class Dimension(val width: Int, val height: Int) {
    def center: FractPixel = FractPixel( (height - 1) / 2, (width -1) / 2 )
  }

  //TODO -- remove redundancy in operator definitions by using a Trait for Vector operations

  /**
   * A type for fractional Pixels
   *
   */
  case class FractPixel(row: Float, col: Float) {
    def -(other: FractPixel) : FractPixel = FractPixel(this.row - other.row, this.col - other.col)
    def +(other: FractPixel) : FractPixel = FractPixel(this.row + other.row, this.col + other.col)
    def *(scale: Float) : FractPixel = FractPixel(row*scale, col*scale)
    def /(scale: Float) : FractPixel = FractPixel(row/scale, col/scale)
    def toPixel : Pixel = Pixel(row.toInt, col.toInt)
  }

  case class RectilinearCoordinate(val x: Double, val y: Double) {
    def -(other: RectilinearCoordinate) : RectilinearCoordinate = RectilinearCoordinate(this.x - other.x, this.y - other.y)
    def +(other: RectilinearCoordinate) : RectilinearCoordinate = RectilinearCoordinate(this.x + other.x, this.y + other.y)
    def *(scale: Float) : RectilinearCoordinate = RectilinearCoordinate(x*scale, y*scale)
    def /(scale: Float) : RectilinearCoordinate = RectilinearCoordinate(x/scale, y/scale)
  }

  /**
   * a Pixel refers to a single raster cell by row (first component) and column (second component).
   *
   * <p>Spatially, {@code Pixels} correspond to the centers of the raster cells </p>
   *
   */
  sealed case class Pixel(row: Int, col: Int) {
    def -(other: Pixel) : Pixel = Pixel(this.row - other.row, this.col - other.col)
    def + (other: Pixel) :Pixel = Pixel(this.row + other.row, this.col + other.col)
    def *(scale: Int) : Pixel = Pixel(row*scale, col*scale)
    def /(scale: Int) : Pixel = Pixel(row/scale, col/scale)
    def toFractPixel : FractPixel = FractPixel(row.toInt, col.toInt)
  }

  /**
   * A viewing angle
   * @param h the horizontal angle (will be normalized to within in the range [-Pi, Pi])
   * @param v the vertical angle (will be normalized to within in the range [0 , Pi])
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



