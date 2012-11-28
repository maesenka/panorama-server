package org.geolatte.panoramaserver

/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/22/12
 */


import scala.math._


import Types._

trait Interpolator {
  def apply(pixel: FractPixel, raster: Raster): Datum
}


trait Padder {
  def apply(row: Int, col: Int, src: Raster): Datum
}

case class ConstPadder(constant: Int) extends Padder {
  def apply(x: Int, y: Int, src: Raster) : Datum = {
    if (y < 0 || y >= src.height || x < 0 || x >= src.width) new ConstantDatum(src.numBands, constant)
    else src(x,y)
  }
}

case class NearestNeighbor(val padder: Padder ) extends Interpolator {
  def apply(pixel: FractPixel, raster: Raster): Datum =
    padder(round(pixel.x).toInt, round(pixel.y).toInt, raster)
}

case class Bilinear (val padder: Padder) extends Interpolator {
  def apply(pixel: FractPixel, raster: Raster): Datum = {
    val u = floor(pixel.x).toInt
    val v = floor(pixel.y).toInt
    val a = pixel.x - u
    val b = pixel.y - v
    val A = padder(u,v, raster)
    val B = padder(u+1,v, raster)
    val C = padder(u,v+1, raster)
    val D = padder(u+1,v+1, raster)
    val result = new Array[Int](A.numBands)
    for( comp <- 0 until result.length) {
      val e = A(comp) + a*(B(comp) - A(comp))
      val f = C(comp) + a*(D(comp) - C(comp))
      result(comp) = round(e + b*(f-e)).toInt
    }
    GenericDatum(result)
  }
}