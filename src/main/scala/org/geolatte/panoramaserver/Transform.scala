package org.geolatte.panoramaserver

/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/23/12
 */

import scala.math._
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
   * @param imageDim the { @code Dimension} of the viewport
   * @param scale the scale of the viewport, such that 1 px of imageViewPort correspond to scale px of the image
   * @return
   */
  def createImage2ViewPort(center: FractPixel, imageDim: Dimension, scale: Float): Pixel => FractPixel = {
    val translation = (imageDim.center - center / scale)
    pixel => pixel.toFractPixel / scale + translation
  }

  /**
   * Creates a transform from image pixels to pixels  of a viewport or canvas that is centered on a specified image pixel
   *
   * @param center image pixel at the center of the view port
   * @param imageDim the { @code Dimension} of the viewport
   * @param scale the scale of the viewport, such that 1 px of imageViewPort correspond to scale px of the image
   * @return
   */
  def createViewPort2Image(center: FractPixel, imageDim: Dimension, scale: Float): Pixel => FractPixel = {
    val translation = center - (imageDim.center * scale)
    pixel => pixel.toFractPixel * scale + translation
  }


  def createEquirect2Rectilinear(viewingCenter: ViewingAngle): ViewingAngle => RectilinearCoordinate = {
    val sinCLat = sin(viewingCenter.vertical)
    val cosCLat = cos(viewingCenter.vertical)
    angle => {
      val cosC = sinCLat * sin(angle.vertical) + cosCLat * cos(angle.horizontal - viewingCenter.horizontal)
      RectilinearCoordinate(
        cos(angle.vertical) * sin(angle.horizontal - viewingCenter.horizontal) / cosC,
        (cosCLat * sin(angle.vertical) - sinCLat * cos(angle.vertical) * cos(angle.horizontal - viewingCenter.horizontal)) / cosC
      )
    }
  }

  def translate(distVector: FractPixel) : Pixel => FractPixel =
    p => p.toFractPixel + distVector

  def scale(scale: Float) : Pixel => FractPixel =
      p => p.toFractPixel * scale


}
