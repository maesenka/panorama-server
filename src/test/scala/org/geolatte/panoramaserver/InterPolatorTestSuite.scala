package org.geolatte.panoramaserver

import org.scalatest.FunSuite

/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/23/12
 */
class InterPolatorTestSuite  extends FunSuite {

  val rasterData = Array.tabulate(10,20)( (r,c) => new MockDatum(r,c))
  val testRaster = new MockRaster(rasterData)

//  println(testRaster)

  test("Nearest-neighbor of (1.2, 2.4) equals (1,2)") {
    val interp = new NearestNeighbor(ConstPadder(0))
    assert( testRaster(1,2) === interp((1.2f, 2.4f), testRaster) )
  }

  test("Nearest-neighbor of (1.8, 2.6) equals (2,3)") {
    val interp = new NearestNeighbor(ConstPadder(0))
    assert( testRaster(2,3) === interp((1.8f, 2.6f), testRaster) )
  }

  test("Nearest-neighbor of (1, 2) equals (1,2)") {
    val interp = new NearestNeighbor(ConstPadder(0))
    assert( testRaster(1,2) === interp((1f, 2f), testRaster) )
  }

  test("bilinear interpolation of (1.3, 1.6) equals (1, 2)") {
    val interp = new Bilinear(ConstPadder(0))
    val datum = interp((1.3f, 1.6f), testRaster)
    assert( 1 === datum(0) )
    assert( 2 === datum(1) )
  }

  test("bilinear of (1, 1) equals (1, 1)") {
      val interp = new Bilinear(ConstPadder(0))
      val datum = interp((1f, 1f), testRaster)
      assert( 1 === datum(0) )
      assert( 1 === datum(1) )
    }

}
