package org.geolatte.panoramaserver

import org.geolatte.panoramaserver.Types.Pixel


/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/23/12
 */



/**
 * Mock Datum that has two bands, first band is the X-coordinate, second the Y-coordinate
 * @param datum
 */
case class MockDatum(val datum: Pixel) extends Datum{
  def numBands: Int = 2

  override def apply(i: Int): Int = i match {
    case 0 => datum.row
    case 1 => datum.col
  }

  override def toString = "(" + datum.row + "," + datum.col + ")"
}

case class MockRaster(val data: Array[Array[MockDatum]]) extends Raster {

  def numBands = 2

  def apply(pixel: Pixel) : Datum = data(pixel.row)(pixel.col)

  def width: Int = data(0).length

  def height: Int = data.length

  override def toString = {
    val rows = for ( i <- 0 until height) yield (data(i) mkString ",")
    rows mkString "\n"
  }

  def update(pixel: Pixel, datum: Datum) : Unit =  throw new UnsupportedOperationException

  def createCompatibleRaster: Raster = {
    new MockRaster(Array.tabulate(height, width)((r,c) => MockDatum(r, c)))
  }
}


