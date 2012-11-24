package org.geolatte.panorama

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
    assert( ViewingAngle(-Pi, 0) === eq2angle(0, 0))
    assert( ViewingAngle(Pi, 0) === eq2angle(0, 20))
    assert( ViewingAngle(0, Pi / 2) === eq2angle(5, 10))
    assert( ViewingAngle(0, Pi) === eq2angle(10, 10))
    assert( ViewingAngle(Pi, Pi) === eq2angle(10, 20))
  }

  test("Equirectangular to ImagePixel-- common tests") {
    val f = createEquiRect2ImgPixel(pixelAngle)
    assert(FractPixel(0, 0) === f(-Pi, 0.0))
    assert(FractPixel(0, 20) === f(Pi, 0.0))
    assert(FractPixel(5, 10) === f(0.0, Pi / 2))
    assert(FractPixel(10, 10) === f(0.0, Pi))
    assert(FractPixel(10, 20) === f(Pi, Pi))
  }

}
