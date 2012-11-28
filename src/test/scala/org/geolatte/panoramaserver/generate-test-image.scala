
import scala.math._
import java.awt.{BasicStroke, Stroke, Color, Graphics2D}
import java.awt.image.{Raster, BufferedImage}
import java.io.File
import javax.imageio.ImageIO


/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/22/12
 */

object TestImageGenerator extends App {
  println("Starting defining image")

  type Pixel = (Int, Int)
  type LonLat = (Double, Double)

  def fillImage(img: BufferedImage) : Unit = {
    val cols = 0 until img.getWidth
    val rows = 0 until img.getHeight
    for (c <- cols; r <- rows)  {
      val red: Float = c.toFloat / img.getWidth.toFloat
      val green: Float = r.toFloat / img.getHeight.toFloat
      val blue: Float = 0.0f
      img.setRGB(c,r, new Color(red,green, blue).getRGB)
    }
  }

  def LonLatToPixel (imgWidth: Int)(lonLat: LonLat) : Pixel = {
    val pixelAngle = 360.toDouble / imgWidth.toDouble
    ( ((lonLat._1 + 180) / pixelAngle).toInt, (lonLat._2 / pixelAngle).toInt )
  }

  def pixelToLonLat (imgWidth: Int)(pixel: Pixel) : LonLat = {
      val pixelAngle = 360.toDouble / imgWidth.toDouble
      ( -180 + ( pixel._1 * pixelAngle ), pixel._2 * pixelAngle )
  }

  def paintGrid(img: BufferedImage) : Unit = {
    val latitudes = 0 to 180 by 15
    val longitudes = -180 to 180 by 15
    val g2: Graphics2D = img.getGraphics.asInstanceOf[Graphics2D];
    g2.setStroke(new BasicStroke(10))
    g2.setColor(Color.BLACK)
    val ll2Pixel = LonLatToPixel(img.getWidth)_

    for (lat <- latitudes){
      var strt = ll2Pixel(longitudes.min, lat)
      var end = ll2Pixel(longitudes.max, lat)
      g2.drawLine(strt._1, strt._2, end._1, end._2)
    }

    for (lon <- longitudes) {
      var strt = ll2Pixel(lon, 0)
      var end = ll2Pixel(lon, 180)
      g2.drawLine(strt._1, strt._2, end._1, end._2)
    }

  }

  def reportSamples(img: BufferedImage, x: Int, y: Int, w: Int, h: Int) : Unit = {
    val samples = img.getRaster.getPixels(x,y,w,h, null.asInstanceOf[Array[Double]])
    println("Number of samples " + samples.length )
    println("SampleModel : " + img.getSampleModel)
    println("Number of Bands: " + img.getSampleModel.getNumBands)
    for (s <- samples) {
      println("sample : " + s)
    }
  }
  val outputFile = new File("/tmp/test-image.png")
  val bufferedImage = new BufferedImage(4800, 2400, BufferedImage.TYPE_INT_RGB )

  fillImage(bufferedImage)
  paintGrid(bufferedImage)

//  reportSamples(bufferedImage, 1000, 1000, 10, 10)

  //ImageIO.write(bufferedImage, "PNG", outputFile)


}


