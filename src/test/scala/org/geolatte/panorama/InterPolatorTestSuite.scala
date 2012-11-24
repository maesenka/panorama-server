package org.geolatte.panorama

import org.scalatest.FunSuite
import org.geolatte.panoramaserver._
import Types._

/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/23/12
 */
class InterPolatorTestSuite  extends FunSuite {

  val rasterData = Array.tabulate(10,20)( (r,c) => new MockDatum(r,c))
  val testRaster = new MockRaster(rasterData)

//  println(testRaster)

  test("Nearest-neighbor of (1.2, 2.4) equals (1,2)") {
    val interp = new NearestNeighbor
    assert( testRaster(1,2) === interp((1.2f, 2.4f), testRaster) )
  }

  test("Nearest-neighbor of (1.8, 2.6) equals (2,3)") {
    val interp = new NearestNeighbor
    assert( testRaster(2,3) === interp((1.8f, 2.6f), testRaster) )
  }

  test("Nearest-neighbor of (1, 2) equals (1,2)") {
    val interp = new NearestNeighbor
    assert( testRaster(1,2) === interp((1f, 2f), testRaster) )
  }

  test("bilinear interpolation of (1.5, 1.5) equals (1.5, 1.5)") {
    val interp = new Bilinear
    val datum = interp((1.5f, 1.5f), testRaster)
    assert( 1.5 === datum(0) )
    assert( 1.5 === datum(1) )
  }

  test("bilinear of (1, 1) equals (1, 1)") {
      val interp = new Bilinear
      val datum = interp((1f, 1f), testRaster)
      assert( 1 === datum(0) )
      assert( 1 === datum(1) )
    }

}
