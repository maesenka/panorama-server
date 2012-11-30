package org.geolatte.panoramaserver


import scala.math._

/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/23/12
 */
object Types {

  /**
   * A 3x3 transformation matrix
   */
  case class TransformMatrix [ S <: Coordinate, D <: Coordinate]  (
    m00: Double, m01: Double, m02: Double,
    m10: Double, m11: Double, m12: Double ) (implicit cf : CoordinateFactory[D]){

    def * (c : S) : D = cf( m00*c.x + m01*c.y + m02, m10*c.x + m11*c.y + m12)

    def * [E <: Coordinate] (other : TransformMatrix[E,S]) = TransformMatrix[E,D] (
      m00*other.m00 + m01*other.m10, m00*other.m01 + m01*other.m11, m00*other.m02 + m01*other.m12 + m02,
      m10*other.m00 + m11*other.m10, m10*other.m01 + m11*other.m11, m10*other.m02 + m11*other.m12 + m12
    )

    def inverse(implicit cf : CoordinateFactory[S]): TransformMatrix[D, S] = {
      val h = m11 * m00 - m10 * m01
      new TransformMatrix[D, S](
        m11 / h, m01 / h, (m01 * m12 - m11 * m02) / h,
        -m10 / h,m00 / h, (m10 * m02 - m00 * m12) / h)
    }
  }

  /**
   * Dimension (width and height) of an Image with top-left coordinates origin
   */
  case class Dimension(origin: Pixel, width: Int, height: Int) {
    def center: FractPixel = FractPixel( origin.x + width/2, origin.y + height/2)
  }

  case class AreaOfInterest[C <: Coordinate](topleft: C, bottomright: C)(implicit cf: CoordinateFactory[C]) {
    def width = bottomright.x - topleft.x + 1
    def height = bottomright.y - topleft.y + 1
    def center = cf(topleft.x + width/2, topleft.y + height/2)
  }

  trait Coordinate {
    type T <: Coordinate

    val x: Double
    val y: Double

    def -(other: T)(implicit cf: CoordinateFactory[T]): T = cf(this.x - other.x, this.y - other.y)

    def +(other: T)(implicit cf: CoordinateFactory[T]): T =  cf(this.x + other.x, this.y + other.y)

    def *(scale: Double)(implicit cf: CoordinateFactory[T]): T = cf(x * scale, y * scale)

    def /(scale: Double)(implicit cf: CoordinateFactory[T]): T = cf(x / scale, y / scale)

  };

  /**
   * A type for fractional Pixels
   *
   */
  case class FractPixel(x: Double, y: Double) extends Coordinate {
    type T = FractPixel
    def toPixel: Pixel = Pixel(round(x).toInt, round(y).toInt)
  }

  case class Rectilinear(x: Double, y: Double) extends Coordinate {
    type T = Rectilinear
  }

  /**
   * a Pixel refers to a single raster cell by row (first component) and column (second component).
   *
   * <p>Spatially, {@code Pixels} corresponds to the topleft-point of the raster cells </p>
   *
   */
  case class Pixel(x: Int, y: Int) {
    def toFractPixel: FractPixel = FractPixel(x, y)
  }

  /**
   * An coordinate in angular units (radians).
   *
   * @param h the horizontal angle (will be normalized to within in the range [-Pi, Pi)) or longitude
   * @param v the vertical angle (will be normalized to within in the range [-Pi/2 , Pi/2)) or latitude
   */
  case class LonLat(private val h: Double, private val v: Double) extends Coordinate {
    type T = LonLat

    private def wrapHorizontal(v: Double) = {
      val rem = v % Pi
      if (rem < 0) Pi + rem
      else -Pi + rem
    }

    private def wrapVertical(v: Double) = {
      val rem = v % (Pi/2)
      if (rem <= 0) (Pi/2) + rem
      else -Pi/2 + rem
    }

    val x = if (h < -Pi || h >= Pi) wrapHorizontal(h) else h
    val y = if (v <= -Pi/2 || v > Pi/2) wrapVertical(v) else v

    val lon = x //longitude synonym for x
    val lat = y //latitude synonym for y
  }

  trait CoordinateFactory[D <: Coordinate] {
    def apply(x: Double, y: Double) : D
  }

  object LonLat {
    implicit def pair2LonLat(pair: (Double, Double)): LonLat = LonLat(pair._1, pair._2)
    implicit val factory = new CoordinateFactory[LonLat] { def apply(x: Double, y: Double) = LonLat(x,y) }
  }

  object Pixel {
    implicit def pair2Pixel(pair: (Int, Int)): Pixel = Pixel(pair._1, pair._2)
    implicit def pixel2FractPixel(p: Pixel): FractPixel = FractPixel(p.x, p.y)
  }

  object FractPixel{
    implicit def pairDouble2FractPixel(pair: (Double, Double)): FractPixel = FractPixel(pair._1, pair._2)
    implicit val factory = new CoordinateFactory[FractPixel] { def apply(x: Double, y: Double)= FractPixel(x,y) }
  }

  object Rectilinear {
    implicit def pairDouble2Rectilinear(pair: (Double, Double)): Rectilinear = Rectilinear(pair._1, pair._2)
    implicit val factory = new CoordinateFactory[Rectilinear] { def apply(x: Double, y: Double)= Rectilinear(x,y) }
  }

}



