package org.geolatte.panorama

import org.geolatte.panoramaserver.{Raster, Types, Datum}

/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/23/12
 */

import Types._

/**
 * Mock Datum that has two bands, first band is the X-coordinate, second the Y-coordinate
 * @param datum
 */
case class MockDatum(val datum: Pixel) extends Datum{
  def numBands: Int = 2

  def apply(i: Int): Double = i match {
    case 0 => datum.row
    case 1 => datum.col
  }

  override def toString = "(" + datum.row + "," + datum.col + ")"
}

case class MockRaster(val data: Array[Array[MockDatum]]) extends Raster[MockDatum] {

  def apply(pixel: Pixel) : MockDatum = data(pixel.row)(pixel.col)

  def width: Int = data(0).length

  def height: Int = data.length

  override def toString = {
    val rows: Traversable[String] = for ( i <- 0 until height)
                  yield (data(i) mkString ",")
    rows mkString "\n"
  }

}

