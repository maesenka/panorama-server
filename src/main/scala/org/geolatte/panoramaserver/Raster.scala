package org.geolatte.panoramaserver

/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/23/12
 */

import Types._
import java.awt.image.{ColorModel, BufferedImage}
import org.geolatte.panoramaserver.TransformFactory.InverseMapper


trait Raster extends Traversable[Pixel]{

  def apply(pixel: Pixel): Datum

  def update(pixel: Pixel, datum: Datum): Unit

  def width: Int

  def height: Int

  def numBands: Int

  def dimension: Dimension

  def foreach[U](f: (Pixel) => U) =  {
    for (x <- 0 until width; y <- 0 until height) {
          f(Pixel(x,y))
    }
  }

  def createCompatibleRaster(width: Int = this.width, height: Int = this.height) : Raster

  def applyOperation(f: Datum => Datum): Raster = {
    val dest = createCompatibleRaster()
    this foreach {p => dest(p) = f(this(p))}
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
      bands(i) = img.getRaster.getSample(pixel.x, pixel.y, i)
    }
    GenericDatum(bands)
  }

  def update(pixel: Pixel, datum: Datum): Unit =
    for (b <- 0 until numBands)
      img.getRaster.setSample(pixel.x, pixel.y, b, datum(b))

  def width: Int = img.getWidth

  def height: Int = img.getHeight

  def dimension : Dimension = Dimension(Pixel(0,0), width, height)


  def createCompatibleRaster (width: Int = this.width, height: Int = this.height) : BufferedImageWrapper =
     new BufferedImageWrapper(new BufferedImage(
       this.img.getColorModel,
       this.img.getColorModel.createCompatibleWritableRaster(width, height),
       this.img.getColorModel.isAlphaPremultiplied,
       null
     ))



}
