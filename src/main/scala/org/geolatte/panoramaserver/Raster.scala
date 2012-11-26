package org.geolatte.panoramaserver

/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/23/12
 */

import Types._
import java.awt.image.{ColorModel, BufferedImage}


trait Raster extends Traversable[Pixel]{

  def apply(pixel: Pixel): Datum

  def update(pixel: Pixel, datum: Datum): Unit

  def width: Int

  def height: Int

  def numBands: Int

  def foreach[U](f: (Pixel) => U) =  {
    for (r <- 0 until height; c <- 0 until width) {
          f(Pixel(r, c))
    }
  }

  def createCompatibleRaster : Raster

  def applyOperation(f: Datum => Datum): Raster = {
    val dest = createCompatibleRaster
    for (r <- 0 until height; c <- 0 until width) {
      val p = Pixel(r, c)
      dest(p) = f(this(p))
    }
    dest
  }

  override def equals(other: Any): Boolean =
      other match {
        case that: Raster => {
          val f: Pixel => Boolean = (p:Pixel) => that(p) == this(p)
          this.width == that.width && this.height == that.height && this.forall( f )
        }
        case _ => false
      }

}

class BufferedImageWrapper(val img: BufferedImage) extends Raster {
  val numBands = img.getSampleModel.getNumBands

  def apply(pixel: Pixel): GenericDatum = {
    val bands = new Array[Int](numBands)
    for (i <- 0 until numBands) {
      bands(i) = img.getRaster.getSample(pixel.col, pixel.row, i)
    }
    GenericDatum(bands)
  }

  def update(pixel: Pixel, datum: Datum): Unit =
    for (b <- 0 until numBands)
      img.getRaster.setSample(pixel.col, pixel.row, b, datum(b))

  def width: Int = img.getWidth

  def height: Int = img.getHeight


  def createCompatibleRaster: BufferedImageWrapper =
     new BufferedImageWrapper(new BufferedImage(
       this.img.getColorModel,
       this.img.getColorModel.createCompatibleWritableRaster(width, height),
       this.img.getColorModel.isAlphaPremultiplied,
       null
     ))



}
