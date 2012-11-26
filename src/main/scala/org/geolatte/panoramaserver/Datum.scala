package org.geolatte.panoramaserver

/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/23/12
 */
trait Datum {
  def numBands : Int
  def apply(band: Int): Int

  def sameElements(that: Datum) : Boolean =  {
    var same = true
    var i = 0
    while (i < numBands && same){
      same = same && this(i) == that(i)
      i += 1
    }
    same
  }

  override def equals(other: Any): Boolean = other match {
    case that: Datum => (this.numBands == that.numBands) && (this sameElements that)
    case _ => false
  }
}

case class RGBDatum(val r: Int, val g: Int, val b: Int) extends Datum {
  def numBands = 3
  override def apply(i: Int) : Int = i match {
    case 0 => r
    case 1 => g
    case 2 => b
    //TODO make sure we don't throw exceptions
    case _ => throw new IndexOutOfBoundsException
  }
}

case class GenericDatum(val bands: Array[Int]) extends Datum {
   def numBands = bands.length
   override def apply(i: Int): Int = bands(i)
}

case class ConstantDatum(val numBands: Int, val constant: Int) extends Datum{
  def apply(band: Int): Int = constant
}