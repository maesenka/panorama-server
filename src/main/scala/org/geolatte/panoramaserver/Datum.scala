package org.geolatte.panoramaserver

/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/23/12
 */
trait Datum {

  def numBands : Int

  def apply(i: Int): Double

}

case class RGBDatum(val r: Float, val g: Float, val b: Float) extends Datum {
  def numBands = 3
  def apply(i: Int) : Double = i match {
    case 0 => r
    case 1 => g
    case 2 => b
    //TODO make sure we don't throw exceptions
    case _ => throw new IllegalArgumentException
  }
}

case class GenericDatum(val bands: Array[Double]) extends Datum {
   def numBands = bands.length
   def apply(i: Int): Double = bands(i)
}