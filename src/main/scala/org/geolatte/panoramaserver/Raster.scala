package org.geolatte.panoramaserver

/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/23/12
 */

import Types._
import java.awt.image.BufferedImage

trait Raster[+T <: Datum] {
  def apply(pixel: Pixel): T
  def width: Int
  def height: Int
}

class BufferedImageWrapper(img: BufferedImage) extends Raster[GenericDatum] {
  def apply(pixel: Pixel): GenericDatum = {
    val pixelArr = img.getRaster.getPixel(pixel.row, pixel.col, null.asInstanceOf[Array[Double]])
    new GenericDatum(pixelArr)
  }

  def width: Int = img.getWidth

  def height: Int = img.getHeight
}