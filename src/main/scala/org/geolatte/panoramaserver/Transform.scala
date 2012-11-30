package org.geolatte.panoramaserver

import org.geolatte.panoramaserver.Types._
import org.geolatte.panoramaserver.Types.TransformMatrix


/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/29/12
 */
trait Transform[S <: Coordinate, D <: Coordinate] {

  val forward : S => D
  val inverse : D => S

  def destinationDimension (srcRegion: AreaOfInterest[S]) : AreaOfInterest[D] = {
    val dTopleft = forward(srcRegion.topleft)
    val dBottomright = forward(srcRegion.bottomright)
    AreaOfInterest[D](dTopleft, dBottomright)
  }

  def append[E <: Coordinate](nextTransform: Transform[D,E]) : Transform[S,E] = TransformPipeline(this, nextTransform)
}

case class AffineTransform[S <: Coordinate, D <: Coordinate](forwardMatrix : TransformMatrix[S,D],
                                                             inverseMatrix: TransformMatrix[D,S]) extends Transform[S, D]{
  val forward : S => D  = (s : S) => forwardMatrix * s
  val inverse : D => S = (d : D) => inverseMatrix * d
}


case class GeneralTransform[ S <: Coordinate, D <: Coordinate](forward : S => D, inverse : D => S)


case class TransformPipeline[S <: Coordinate, B <: Coordinate ,D <: Coordinate](first: Transform[S, B], next: Transform[B,D]) extends Transform[S,D]{
  val forward: (S) => D = first.forward andThen next.forward
  val inverse: (D) => S = next.inverse andThen first.inverse
}


