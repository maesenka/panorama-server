package org.geolatte.panoramaserver

/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/22/12
 */


import scala.math._


import Types._

trait Interpolator {
  def apply[T <: Datum](pixel: FractPixel, raster: Raster[T]): Datum
}

class NearestNeighbor extends Interpolator {

  def toIntPixel(inp: FractPixel) = Pixel(round(inp.row), round(inp.col))

  def apply[T <: Datum](pixel: FractPixel, raster: Raster[T]) =
    raster(toIntPixel(pixel))

}

class Bilinear[T <: Datum] extends Interpolator {
  def apply[T <: Datum](pixel: FractPixel, raster: Raster[T]): GenericDatum = {
    val u = floor(pixel.row).toInt
    val v = floor(pixel.col).toInt
    val a = pixel.row - u
    val b = pixel.col - v
    val A = raster(u,v)
    val B = raster(u+1,v)
    val C = raster(u,v+1)
    val D = raster(u+1,v+1)
    val result = new Array[Double](A.numBands)
    for( comp <- 0 until result.length) {
      val e = A(comp) + a*(B(comp) - A(comp))
      val f = C(comp) + a*(D(comp) - C(comp))
      result(comp) = e + b*(f-e)
    }
    GenericDatum(result)
  }
}