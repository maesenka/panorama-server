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
    assert( -Pi === ViewingAngle(-Pi, Pi/2).horizontal)
    assert( (Pi - 1) === ViewingAngle(- Pi - 1, Pi/2).horizontal)
    assert( (Pi - 0.01) === ViewingAngle(- 3*Pi - 0.01, Pi/2).horizontal)
  }

  test("Horizontal angle wraps right to left") {
    assert( Pi === ViewingAngle(Pi, Pi/2).horizontal)
    assert( (-Pi + 1) === ViewingAngle(Pi + 1, Pi/2).horizontal)
    assert( (-Pi + 0.01) === ViewingAngle(3*Pi + 0.01, Pi/2).horizontal)
  }

  test("Vertical angle wraps top to bottom") {
    assert( 0 === ViewingAngle(0, 0).vertical)
    assert( Pi - 1 === ViewingAngle(0, -1).vertical)
    assert( (Pi - 0.01) === ViewingAngle(0, - 3*Pi - 0.01).vertical)
  }

  test("Horizontal angle wraps bottom to top") {
    assert( Pi === ViewingAngle(0, Pi).vertical)
    assert( 1 === ViewingAngle(0, Pi + 1).vertical)
    assert( 1 === ViewingAngle(0, 3*Pi + 1).vertical)
  }


  test("Dimension correctly calculates its center") {
    assert( FractPixel(2.0f, 4.0f) === Dimension(9, 5).center)
  }

}
