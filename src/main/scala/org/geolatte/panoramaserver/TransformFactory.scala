package org.geolatte.panoramaserver

/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/23/12
 */

import scala.math._
import Types._


object TransformFactory {

  def mkPixel2EquirectTransform(imgDimension: Dimension, yawDeviation: Double = 0.0) : AffineTransform[FractPixel, LonLat] = {
    val pixelAngle = 2*Pi/imgDimension.width
    val forwardTM = TransformMatrix[FractPixel, LonLat](pixelAngle, 0.0, -Pi + yawDeviation, 0.0, -pixelAngle, Pi/2)
    AffineTransform(forwardTM, forwardTM.inverse)
  }

  /**
   * creates a Projection function from an image Pixel to a ViewingAngle (EquiRectangular coordinates)
   *
   * <p>This function assumes an image model where top-left corresponds to ViewingAngle(-Pi, 0)</p>
   *
   * @param pixelAngle
   * @param yawDeviation
   * @return
   */
  def createImgPixel2Equirect(pixelAngle: Double, yawDeviation: Double = 0.0): FractPixel => LonLat =
    pixel => new LonLat(-Pi + yawDeviation + (pixel.x * pixelAngle), Pi / 2 - pixel.y * pixelAngle)

  /**
   * creates a Projection function from a ViewingAngle (EquiRectangular coordinates) to an image Pixel.
   *
   * <p>This function assumes an image model where top-left corresponds to ViewingAngle(-Pi, 0)</p>
   *
   * @param pixelAngle
   * @param yawDeviation
   * @return
   */
  def createEquirect2ImgPixel(pixelAngle: Double, yawDeviation: Double = 0.0): LonLat => FractPixel =
    angle => new FractPixel(
      ((angle.lon + Pi - yawDeviation) / pixelAngle),
      ((Pi / 2 - angle.lat) / pixelAngle)
    )

  /**
   * Creates a transform from image pixels to pixels of a viewport or canvas that is centered on a specified image pixel
   *
   * @param center image pixel at the center of the view port
   * @param imageDim the { @code Dimension} of the viewport
   * @param scale the scale of the viewport, such that 1 px of imageViewPort correspond to scale px of the image
   * @return
   */
  def createImage2ViewPort(center: FractPixel, imageDim: Dimension, scale: Float): FractPixel => FractPixel = {
    val translation = (imageDim.center - center / scale)
    pixel => pixel / scale + translation
  }

  /**
   * Creates a transform from image pixels to pixels  of a viewport or canvas that is centered on a specified image pixel
   *
   * @param center image pixel at the center of the view port
   * @param imageDim the { @code Dimension} of the viewport
   * @param scale the scale of the viewport, such that 1 px of imageViewPort correspond to scale px of the image
   * @return
   */
  def createViewPort2Image(center: FractPixel, imageDim: Dimension, scale: Float): FractPixel => FractPixel = {
    val translation = center - (imageDim.center * scale)
    pixel => pixel * scale + translation
  }


  def createEquirect2Rectilinear(viewingCenter: LonLat, radius: Double): LonLat => FractPixel = {
    val sinCLat = sin(viewingCenter.lat)
    val cosCLat = cos(viewingCenter.lat)
    angle => {
      val cosC = sinCLat * sin(angle.lat) + cosCLat * cos(angle.lat) * cos(angle.lon - viewingCenter.lon)
      FractPixel(
        radius * cos(angle.lat) * sin(angle.lon - viewingCenter.lon) / cosC,
        radius * (cosCLat * sin(angle.lat) - sinCLat * cos(angle.lat) * cos(angle.lon - viewingCenter.lon)) / cosC
      )
    }
  }

  def createRectilinear2Equirect(viewingCenter: LonLat, radius: Double): FractPixel => LonLat =
    rc => {
      val theR = sqrt(rc.x * rc.x + rc.y * rc.y)
      val theC = atan2(theR, radius)
      val cosC = cos(theC)
      val sinC = sin(theC)
      val cosCLat = cos(viewingCenter.lat)
      val sinCLat = sin(viewingCenter.lat)
      LonLat(
        viewingCenter.lon + atan2(rc.x * sinC, theR * cosCLat * cosC - rc.y * sinCLat * sinC),
        asin(cosC * sinCLat + (rc.y * sinC * cosCLat / theR))
      )
    }


  def translate(distVector: FractPixel): FractPixel => FractPixel =
    p => p + distVector

  def scale(scale: Double): FractPixel => FractPixel =
    p => p * scale


  case class InverseMapper[D <: FractPixel](forward: FractPixel => D, inverse: FractPixel => D) {

    def destinationDimension(srcRegion: Dimension): Dimension = {
      val projUL = forward(srcRegion.origin)
      val projLR = forward(FractPixel(srcRegion.origin.x + srcRegion.width, srcRegion.origin.y + srcRegion.height))
      val upperLeft = Pixel(round(min(projUL.x, projLR.x)).toInt, round(min(projUL.y, projLR.y)).toInt)
      val lowerRight = Pixel(round(max(projUL.x, projLR.x)).toInt, round(max(projUL.y, projLR.y)).toInt)
      val r = Dimension(upperLeft, round(lowerRight.x - upperLeft.x), round(lowerRight.y - upperLeft.y))
      r
    }

    def applyInverseMapper(src: Raster, interp: Interpolator, region: Dimension): Raster = {
      val destDim = destinationDimension(region)
      val dest = src.createCompatibleRaster(destDim.width, destDim.height)
      dest foreach {
        (p: Pixel) => {
          val dP = FractPixel(p.x + destDim.origin.x, p.y + destDim.origin.y)
          dest(p) = interp(inverse(dP), src)
        }
      }
      dest
    }

    def applyInverseMapper(src: Raster, interp: Interpolator): Raster = applyInverseMapper(src, interp, src.dimension)
  }

  case class RectilinearInverseMapper(val viewingAngle: LonLat, val hFov: Double, val pixelAngle: Double = (Pi / 2400)) {
    val radius = 1 / (2 * pixelAngle)
    //-- heuristic 2400/(2*Pi)
    val pixel2Equirect = createImgPixel2Equirect(pixelAngle, 0.0d)
    val equirect2pixel = createEquirect2ImgPixel(pixelAngle, 0.0d)
    val equirect2RectiLinear = createEquirect2Rectilinear(viewingAngle, radius)
    val rectilinear2Equirect = createRectilinear2Equirect(viewingAngle, radius)
    //toImagePixel takes into account the difference in orientation of the rectilinear coordinate Y-axis
    val toImagePixel: FractPixel => FractPixel = p => FractPixel(p.x, -p.y)
    val forward: FractPixel => FractPixel = p => toImagePixel(equirect2RectiLinear(pixel2Equirect(p)))
    val inverse: FractPixel => FractPixel = p => equirect2pixel(rectilinear2Equirect(toImagePixel(p)))
    private val mapper = InverseMapper(forward, inverse)
    val FoVUpperLeft = viewingAngle + LonLat(-hFov/2, +hFov/4)
    val FoVLowerRight = viewingAngle + LonLat(hFov/2, -hFov/4)
    val FoVULPixel = equirect2pixel(FoVUpperLeft)
    val FoVLRPixel = equirect2pixel(FoVLowerRight)
    val region = Dimension(FoVULPixel.toPixel, round(FoVLRPixel.x - FoVULPixel.x).toInt, round(FoVLRPixel.y - FoVULPixel.y).toInt)

    def inverseMap(srcRaster: Raster): (Raster, Dimension) = {
      val d = mapper.applyInverseMapper(srcRaster, Bilinear(ConstPadder(0)), region)
      (d, mapper.destinationDimension(region))
    }
  }


}
