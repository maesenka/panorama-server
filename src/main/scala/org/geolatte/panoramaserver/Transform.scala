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
   * <p>This function assumes an image model where top-left corresponds to ViewingAngle(-Pi, 0)</p>
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
   * <p>This function assumes an image model where top-left corresponds to ViewingAngle(-Pi, 0)</p>
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
   * @param imageDim the {@code Dimension} of the viewport
   * @param scale the scale of the viewport, such that 1 px of imageViewPort correspond to scale px of the image
   * @return
   */
  def createImage2ViewPort(center: Pixel, imageDim: Dimension, scale: Double): Pixel => FractPixel =
    pixel => {
      val row = (pixel.row - center.row)/scale + (imageDim.height-1)/2
      val col = (pixel.col - center.col)/scale + (imageDim.width-1)/2
      new FractPixel(row.toFloat, col.toFloat)
    }

  /**
   * Creates a transform from image pixels to pixels  of a viewport or canvas that is centered on a specified image pixel
   *
   * @param center image pixel at the center of the view port
   * @param imageDim the {@code Dimension} of the viewport
   * @param scale the scale of the viewport, such that 1 px of imageViewPort correspond to scale px of the image
   * @return
   */
  def createViewPort2Image(center: Pixel, imageDim: Dimension, scale: Double): Pixel => FractPixel =
    pixel => {
      val row = pixel.row*scale + center.row - (imageDim.height-1)*scale/2
      val col = pixel.col*scale + center.col - (imageDim.width-1)*scale/2
      new FractPixel(row.toFloat, col.toFloat)
    }


}
