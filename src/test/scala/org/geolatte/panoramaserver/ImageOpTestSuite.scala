package org.geolatte.panoramaserver

import org.scalatest.FunSuite
import java.io.File
import javax.imageio.ImageIO
import org.geolatte.panoramaserver.TransformFactory._
import org.geolatte.panoramaserver.Types._
import scala.math._

/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/26/12
 */
class ImageOpTestSuite extends FunSuite with Instrumented {

  val testImageFile = new File("/home/maesenka/workspaces/github/geolatte-panoviewer/panos/5C01K8RM.jpg") //new File("/tmp/test-image.png") //
  val testImage =ImageIO.read(testImageFile)
  val srcRaster = new BufferedImageWrapper(testImage)

  test("Raster creates a compatible dest image from a source image") {
    val dest = srcRaster.createCompatibleRaster()
    assert(testImage.getColorModel === dest.img.getColorModel)
    assert(testImage.getWidth === dest.width)
    assert(testImage.getHeight === dest.height)
    val wrapped = new BufferedImageWrapper(dest.img)
    assert(3 === wrapped.numBands)
  }

//  test("ImageOp of identity op returns the same image") {
//    val dest = timed(srcRaster applyOperation ((d: Datum) => d))
//    assert(dest === srcRaster)
//    ImageIO.write(dest.asInstanceOf[BufferedImageWrapper].img, "PNG", new File("/tmp/out.png"))
//  }

//  test("Compare speed with native") {
//    val trx : AffineTransform = new AffineTransform()
//    trx.setToScale(0.5, 0
//
// .5)
//    val atOp = new AffineTransformOp( trx, AffineTransformOp.TYPE_BICUBIC)
//    val dest = timed(atOp.filter(testImage, null))
//    ImageIO.write(dest,"PNG", new File("/tmp/scaled-atop.png"))
//  }
//
//
//  test("Scaling the test image") {
//    val mapper = InverseMapper(scale(0.5f), scale(2f))
//    val dest = timed(srcRaster applyInverseMapper (mapper, Bilinear(ConstPadder(255))))
//    ImageIO.write(dest.asInstanceOf[BufferedImageWrapper].img, "PNG", new File("/tmp/scaled.png"))
//  }

  test("Rectilinear projection test") {
    val pixelAngle = (2*Pi)/4800
    val viewingAngle = LonLat(0, 0)
    val mapper = new RectilinearInverseMapper(viewingAngle, Pi/2, Dimension((0,0), 4800, 2400) )
    val (dest,destDim) = timed(mapper.inverseMap(srcRaster))
    ImageIO.write(dest.asInstanceOf[BufferedImageWrapper].img, "PNG", new File("/tmp/rectlinear-" + viewingAngle.lon + "-" + viewingAngle.lat +".png"))
  }




}
