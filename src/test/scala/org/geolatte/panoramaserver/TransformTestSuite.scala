package org.geolatte.panoramaserver

import org.scalatest.FunSuite
import scala.math.Pi
import org.geolatte.panoramaserver.Types._
import org.geolatte.panoramaserver.Transform._


/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/23/12
 */
class TransformTestSuite extends FunSuite {

  val pixelAngle = Pi / 10;
  val rasterData = Array.tabulate(10, 20)((r, c) => new MockDatum(r, c))
  val testRaster = new MockRaster(rasterData)

  test("ImagePixel to Equirectangular -- common tests") {
    val eq2angle = createImgPixel2Equirect(pixelAngle)
//    assert( ViewingAngle(-Pi, 0) === eq2angle(0d, 0d))
//    assert( ViewingAngle(Pi, 0) === eq2angle(0d, 20d))
//    assert( ViewingAngle(0, Pi / 2) === eq2angle(5d, 10d))
//    assert( ViewingAngle(0, Pi) === eq2angle(10d, 10d))
//    assert( ViewingAngle(Pi, Pi) === eq2angle(10d, 20d))
  }

  test("Equirectangular to ImagePixel-- common tests") {
    val f = createEquirect2ImgPixel(pixelAngle)
//    assert(FractPixel(0, 0) === f(-Pi, 0.0))
//    assert(FractPixel(0, 20) === f(Pi, 0.0))
//    assert(FractPixel(10, 10) === f(0.0, Pi / 2))
//    assert(FractPixel(10, 10) === f(0.0, Pi/2))
//    assert(FractPixel(10, 20) === f(Pi, Pi/2))
  }

  test("Viewport2Image: Viewport centered on image, at scale 1") {
    val f = createViewPort2Image((5.0, 10.0), Dimension((0,0), 5,3), 1)
    assert(FractPixel(4,8) === f(0d,0d))
    assert(FractPixel(6,12) === f(2d,4d))
    assert(FractPixel(4,12) === f(0d,4d))
    assert(FractPixel(6,12) === f(2d,4d))
    assert(FractPixel(5,10) === f(1d,2d))
  }

  test("Viewport2Image: Viewport centered on image, at scale 2") {
    val f = createViewPort2Image((5.0, 10.0), Dimension((0,0), 5,3), 2)
    assert(FractPixel(3,6) === f(0d,0d))
    assert(FractPixel(7,14) === f(2d,4d))
    assert(FractPixel(3,14) === f(0d,4d))
    assert(FractPixel(7,14) === f(2d,4d))
    assert(FractPixel(5,10) === f(1d,2d))
  }

  test("Image2ViewPort: Viewport centered on image, at scale 1") {
    val f = createImage2ViewPort((5.0, 10.0), Dimension((0,0),5,3), 1)
    assert(FractPixel(0,0) === f(4d,8d))
    assert(FractPixel(2,4) === f(6d,12d))
    assert(FractPixel(0,4) === f(4d,12d))
    assert(FractPixel(2,4) === f(6d,12d))
    assert(FractPixel (1,2) === f(5d,10d))
  }

  test("Image2ViewPort: Viewport centered on image, at scale 2") {
    val f = createImage2ViewPort((5.0, 10.0), Dimension((0,0),5,3), 2)
    assert(FractPixel(0,0) === f(3d,6d))
    assert(FractPixel(2,4) === f(7d,14d))
    assert(FractPixel(0,4) === f(3d,14d))
    assert(FractPixel(2,4) === f(7d,14d))
    assert(FractPixel(1,2) === f(5d,10d))
  }

}
