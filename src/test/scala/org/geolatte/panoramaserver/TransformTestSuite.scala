package org.geolatte.panoramaserver

import org.scalatest.FunSuite
import scala.math.Pi
import org.geolatte.panoramaserver.Types._
import org.geolatte.panoramaserver.TransformFactory._


/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/23/12
 */
class TransformTestSuite extends FunSuite {

  val pixelAngle = Pi / 10;
  val rasterData = Array.tabulate(10, 20)((r, c) => new MockDatum(c, r))
  val testRaster = new MockRaster(rasterData)

  test("ImagePixel to Equirectangular -- common tests") {
    val eq2angle = mkPixel2EquirectTransform(Dimension((0, 0), 20, 10))
    assert(LonLat(-Pi, Pi / 2) === eq2angle.forward(0d, 0d))
    assert(LonLat(Pi, Pi / 2) === eq2angle.forward(20d, 0d))
    assert(LonLat(0, 0) === eq2angle.forward(10d, 5d))
    assert(LonLat(0, -Pi / 2) === eq2angle.forward(10d, 10d))
    assert(LonLat(Pi, -Pi / 2) === eq2angle.forward(20d, 10d))
  }

  test("Equirectangular to ImagePixel-- common tests") {
    val f = mkPixel2EquirectTransform(Dimension((0, 0), 20, 10)).inverse
    assert(FractPixel(0, 0) === f(-Pi, Pi / 2))
    //wraps to 0,0 due to normalization of LonLat to [-Pi,  Pi]
    assert(FractPixel(0, 0) === f(Pi, Pi / 2))
    assert(FractPixel(10, 5) === f(0.0, 0.0))
    assert(FractPixel(10, 0) === f(0.0, -Pi / 2))
    assert(FractPixel(0, 0) === f(Pi, -Pi / 2))
  }

  test("translate test") {
    val f = Translate[FractPixel, Rectilinear]((2.0d, 3.0d))
    assert(Rectilinear(2, 3) === f.forward(0d, 0d))
    assert(Rectilinear(12, 13) === f.forward(10d, 10d))
    assert(FractPixel(0, 0) === f.inverse(2d, 3d))
  }

  test("scale test") {
    val f = Scale[FractPixel, Rectilinear](4.0d)
    assert(Rectilinear(0, 0) === f.forward(0d, 0d))
    assert(Rectilinear(4, 4) === f.forward(1d, 1d))
    assert(FractPixel(1, 1) === f.inverse(4d, 4d))
  }

  test("translate and then scale") {
    val tr = Translate[FractPixel, Rectilinear]((2.0d, 3.0d))
    val s = Scale[Rectilinear, Rectilinear](4.0d)
    val composed = tr.append(s)
    assert(Rectilinear(8, 12) === composed.forward(0d,0d))
    assert(FractPixel(0d,0d) === composed.inverse(8d,12d))
  }

  test("Scale and then translate") {
    val tr = Translate[Rectilinear, Rectilinear]((2.0d, 3.0d))
    val s : Affine[FractPixel, Rectilinear] = Scale[FractPixel, Rectilinear](4.0d)
    val composed = s.append(tr)
    assert(Rectilinear(6, 3) === composed.forward(1d,0d))
  }


}
