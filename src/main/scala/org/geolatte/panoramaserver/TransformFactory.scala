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

  def mkEquirect2RectilinearTransform(center: LonLat, radius: Double): Transform[LonLat, Rectilinear] = {
    val sinCLat = sin(center.lat)
    val cosCLat = cos(center.lat)
    val forw: LonLat => Rectilinear = angle => {
      val cosC = sinCLat * sin(angle.lat) + cosCLat * cos(angle.lat) * cos(angle.lon - center.lon)
      Rectilinear(
        radius * cos(angle.lat) * sin(angle.lon - center.lon) / cosC,
        radius * (cosCLat * sin(angle.lat) - sinCLat * cos(angle.lat) * cos(angle.lon - center.lon)) / cosC
      )
    }

    val inv: Rectilinear => LonLat = px => {
      val theR = sqrt(px.x * px.x + px.y * px.y)
      val theC = atan2(theR, radius)
      val cosC = cos(theC)
      val sinC = sin(theC)
      val cosCLat = cos(center.lat)
      val sinCLat = sin(center.lat)
      LonLat(
        center.lon + atan2(px.x * sinC, theR * cosCLat * cosC - px.y * sinCLat * sinC),
        asin(cosC * sinCLat + (px.y * sinC * cosCLat / theR))
      )
    }
    new GeneralTransform[LonLat, Rectilinear](forw, inv)
  }

  case class InverseMapper(forward: FractPixel => Rectilinear, inverse: Rectilinear => FractPixel) {

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
          val dP = Rectilinear(p.x + destDim.origin.x, p.y + destDim.origin.y)
          dest(p) = interp(inverse(dP), src)
        }
      }
      dest
    }

    def applyInverseMapper(src: Raster, interp: Interpolator): Raster = applyInverseMapper(src, interp, src.dimension)
  }

  case class RectilinearInverseMapper(val viewingAngle: LonLat, val hFov: Double, srcDimension: Dimension) {
    val pixelAngle = 2*Pi/srcDimension.width
    val radius = 1 / (2 * pixelAngle)
    //-- heuristic 2400/(2*Pi)
    val pixel2Equirect = mkPixel2EquirectTransform(srcDimension).forward
    val equirect2pixel = mkPixel2EquirectTransform(srcDimension).inverse
    val equirect2RectiLinear = mkEquirect2RectilinearTransform(viewingAngle, radius).forward
    val rectilinear2Equirect = mkEquirect2RectilinearTransform(viewingAngle, radius).inverse
//    toImagePixel takes into account the difference in orientation of the rectilinear coordinate Y-axis
    val toImagePixel: Rectilinear => Rectilinear = p => Rectilinear(p.x, -p.y)
    val forward: FractPixel => Rectilinear = p => toImagePixel(equirect2RectiLinear(pixel2Equirect(p)))
    val inverse: Rectilinear => FractPixel = p => equirect2pixel(rectilinear2Equirect(toImagePixel(p)))
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
