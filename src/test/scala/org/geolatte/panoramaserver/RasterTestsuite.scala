package org.geolatte.panoramaserver

import org.scalatest.FunSuite


/**
 * @author Karel Maesen, Geovise BVBA
 *         creation-date: 11/22/12
 */
class RasterTestSuit extends FunSuite {


  val rasterData = Array.tabulate(10, 20)((r, c) => new MockDatum(r, c))
  val testRaster = new MockRaster(rasterData)

  println(testRaster)

  test("raster reports correct size") {
    assert(testRaster.width === 20)
    assert(testRaster.height === 10)
  }

  test("raster access returns correct pixels") {
    for {
      c <- 0 until testRaster.width;
      r <- 0 until testRaster.height
    } assert(testRaster(r, c) === new MockDatum(r, c))
  }

  test("Rasters are equal to themselves") {
    assert(testRaster === testRaster)
  }

  test("raster are equal when having same dimensions and same sample data") {
    assert(testRaster === new MockRaster(Array.tabulate(10, 20)((r, c) => new MockDatum(r, c))))
  }

  test("raster are unequal when having same dimensions but different sample data") {
    assert(testRaster != new MockRaster(Array.tabulate(10, 20)((r, c) => new MockDatum(r+1, c+1))))
  }
}
