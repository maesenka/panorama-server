package org.geolatte.panoramaserver

import org.scalatest.FunSuite
import scala.math.Pi
import org.geolatte.panoramaserver.Types._

/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/24/12
 */
class TypesTestSuite extends FunSuite {

  test("Horizontal angle wraps left to right") {
    assert( -Pi === LonLat(-Pi, Pi/2).lon)
    assert( (Pi - 1) === LonLat(- Pi - 1, Pi/2).lon)
    assert( (Pi - 0.01) === LonLat(- 3*Pi - 0.01, Pi/2).lon)
  }

  test("Horizontal angle wraps right to left") {
    assert( -Pi === LonLat(Pi, Pi/2).lon)
    assert( (-Pi + 1) === LonLat(Pi + 1, Pi/2).lon)
    assert( (-Pi + 0.01) === LonLat(3*Pi + 0.01, Pi/2).lon)
  }

  test("Vertical angle wraps top to bottom") {
    assert( Pi/2 === LonLat(0, Pi/2).lat)
    assert( Pi/2 - 1 === LonLat(0, -Pi/2 -1).lat)
//    assert( (Pi/2 - 0.01) === ViewingAngle(0, - 3*Pi/2 - 0.01).vertical) -- how to test for equality of floats?
  }

  test("Vertical angle wraps bottom to top") {
    assert( Pi/2 === LonLat(0, -Pi/2).lat)
    assert( -Pi/2 + 1 === LonLat(0, Pi/2 + 1).lat)
//    assert( 1 === ViewingAngle(0, 3*Pi + 1).vertical)
  }

  test("Dimension correctly calculates its center") {
    assert( FractPixel(2,1) === Dimension((0,0), 5, 3).center)
  }

}
