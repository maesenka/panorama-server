package org.geolatte.panoramaserver

/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/23/12
 */

import scala.math.Pi
import Types._


object Transform {

  /**
   * creates a Projection function from an image Pixel to a ViewingAngle (EquiRectangular coordinates)
   *
   * @param pixelAngle
   * @param yawDeviation
   * @return
   */
  def createImgPixel2Equirect(pixelAngle: Double, yawDeviation: Double = 0.0): Pixel => ViewingAngle =
    pixel => new ViewingAngle(-Pi + yawDeviation + (pixel.col * pixelAngle), pixel.row * pixelAngle)

  /**
   * creates a Projection function from a ViewingAngle (EquiRectangular coordinates) to an image Pixel.
   *
   * @param pixelAngle
   * @param yawDeviation
   * @return
   */
  def createEquiRect2ImgPixel(pixelAngle: Double, yawDeviation: Double = 0.0): ViewingAngle => FractPixel =
    angle => new FractPixel(
      (angle.vertical / pixelAngle).toFloat,
      ((angle.horizontal + Pi - yawDeviation) / pixelAngle).toFloat
    )

  /**
   * Creates a transform from image pixels to pixels of a viewport or canvas that is centered on a specified image pixel
   *
   * @param center image pixel at the center of the view port
   * @param width width of the viewport
   * @param height height of the viewport
   * @param scale the scale of the viewport, such that 1 px of imageViewPort correspond to scale px of the image
   * @return
   */
  def createImg2ViewPort(center: Pixel, width: Int, height: Int, scale: Double): Pixel => FractPixel =
    pixel => {
      val row = (pixel.row - center.row)/scale + height/2
      val col = (pixel.col - center.col)/scale + width/2
      new FractPixel(row.toFloat, col.toFloat)
    }

  /**
   * Creates a transform from image pixels to pixels  of a viewport or canvas that is centered on a specified image pixel
   *
   * @param center image pixel at the center of the view port
   * @param width width of the viewport
   * @param height height of the viewport
   * @param scale the scale of the viewport, such that 1 px of imageViewPort correspond to scale px of the image
   * @return
   */
  def createViewPort2Img(center: Pixel, width: Int, height: Int, scale: Double): Pixel => FractPixel =
    pixel => {
      val row = pixel.row*scale + center.row - height*scale/2
      val col = pixel.col*scale + center.col - width*scale/2
      new FractPixel(row.toFloat, col.toFloat)
    }


}
