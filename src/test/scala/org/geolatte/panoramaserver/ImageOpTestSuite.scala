package org.geolatte.panoramaserver

import org.scalatest.FunSuite
import org.geolatte.panoramaserver._
import java.io.File
import javax.imageio.ImageIO

/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/26/12
 */
class ImageOpTestSuite extends FunSuite with Instrumented {

  val testImageFile = new File("/home/maesenka/workspaces/github/geolatte-panoviewer/panos/5C01K8RM.jpg")
  val testImage =ImageIO.read(testImageFile)
  val srcRaster = new BufferedImageWrapper(testImage)

  test("ImageOp creates a compatible dest image from a source image") {
    val dest = srcRaster.createCompatibleRaster
    assert(testImage.getColorModel === dest.img.getColorModel)
    assert(testImage.getWidth === dest.width)
    assert(testImage.getHeight === dest.height)
    val wrapped = new BufferedImageWrapper(dest.img)
    assert(3 === wrapped.numBands)
  }

  test("ImageOp of identity op returns the same image") {
    val dest = timed(srcRaster applyOperation ((d: Datum) => d))
    assert(dest === srcRaster)
    ImageIO.write(dest.asInstanceOf[BufferedImageWrapper].img, "PNG", new File("/tmp/out.png"))
  }




}
