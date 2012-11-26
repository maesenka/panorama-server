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
  def apply(row: Int, col: Int, src: Raster) : Datum = {
    if (row < 0 || row >= src.height || col < 0 || col >= src.height) new ConstantDatum(src.numBands, constant)
    else src(row,col)
  }
}

case class NearestNeighbor(val padder: Padder ) extends Interpolator {
  def apply(pixel: FractPixel, raster: Raster) =
    padder(round(pixel.row), round(pixel.col), raster)
}

case class Bilinear (val padder: Padder) extends Interpolator {
  def apply(pixel: FractPixel, raster: Raster): GenericDatum = {
    val u = floor(pixel.row).toInt
    val v = floor(pixel.col).toInt
    val a = pixel.row - u
    val b = pixel.col - v
    val A = padder(u,v, raster)
    val B = padder(u+1,v, raster)
    val C = padder(u,v+1, raster)
    val D = padder(u+1,v+1, raster)
    val result = new Array[Int](A.numBands)
    for( comp <- 0 until result.length) {
      val e = A(comp) + a*(B(comp) - A(comp))
      val f = C(comp) + a*(D(comp) - C(comp))
      result(comp) = round(e + b*(f-e))
    }
    GenericDatum(result)
  }
}