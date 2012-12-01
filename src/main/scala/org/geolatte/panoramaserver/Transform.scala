package org.geolatte.panoramaserver

import org.geolatte.panoramaserver.Types._
import org.geolatte.panoramaserver.Types.TransformMatrix


/**
* @author Karel Maesen, Geovise BVBA
*         creation-date: 11/29/12
*/
trait Transform[S <: Coordinate, D <: Coordinate] {

  val forward: S => D
  val inverse: D => S

  def destinationDimension(srcRegion: AreaOfInterest[S])(implicit cf: CoordinateFactory[D]): AreaOfInterest[D] = {
    val dTopleft = forward(srcRegion.topleft)
    val dBottomright = forward(srcRegion.bottomright)
    AreaOfInterest[D](dTopleft, dBottomright)
  }

  def append[E <: Coordinate](nextTransform: Transform[D, E]): Transform[S, E] = TransformPipeline(this, nextTransform)

}

case class GeneralTransform[S <: Coordinate, D <: Coordinate](forward: S => D, inverse: D => S) extends Transform[S,D]

trait Affine[S <: Coordinate, D <: Coordinate] extends Transform[S,D]{
  val forwardMatrix : TransformMatrix[S,D]
  val inverseMatrix : TransformMatrix[D,S]
  val forward: S => D = (s: S) => forwardMatrix * s
  val inverse: D => S = (d: D) => inverseMatrix * d

  override def append[E <: Coordinate](next: Transform[D, E]): Transform[S, E] = {
    next match {
      case a: Affine[_,_] => new AffineTransform[S,E](a.forwardMatrix * forwardMatrix, inverseMatrix * a.inverseMatrix)
      case _ => super.append(next)
    }
  }

}


case class AffineTransform[S <: Coordinate, D <: Coordinate](forwardMatrix: TransformMatrix[S, D], inverseMatrix: TransformMatrix[D,S])
  extends Affine[S,D]

case class Scale[S <: Coordinate, D <: Coordinate](scale: Double)
  (implicit cfD: CoordinateFactory[D], cfS: CoordinateFactory[S]) extends Affine[S,D] {
  val forwardMatrix = TransformMatrix[S,D](scale, 0.0, 0.0, 0.0, scale, 0.0)
  val inverseMatrix = TransformMatrix[D,S](1.0/scale, 0.0,0.0, 0.0, 1.0/scale, 0.0)
}

case class Translate[S <: Coordinate, D <: Coordinate](dCoord : S)
  (implicit cfD: CoordinateFactory[D], cfS: CoordinateFactory[S]) extends Affine[S,D] {
  val forwardMatrix = TransformMatrix[S,D](1.0, 0.0, dCoord.x, 0.0, 1.0, dCoord.y)
  val inverseMatrix = TransformMatrix[D,S](1.0, 0.0,-dCoord.x, 0.0, 1.0, -dCoord.y)
}


case class TransformPipeline[S <: Coordinate, B <: Coordinate, D <: Coordinate](first: Transform[S, B], next: Transform[B, D]) extends Transform[S, D] {
  val forward: (S) => D = first.forward andThen next.forward
  val inverse: (D) => S = next.inverse andThen first.inverse
}


