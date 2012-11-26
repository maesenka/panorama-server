package org.geolatte.panoramaserver

/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 11/26/12
 */
trait Instrumented {


  def timed[A](f: => A) =  {
    val t0 = System.nanoTime
    val ans = f
    val elapsed = (System.nanoTime-t0)*1e-9;
    val e = new RuntimeException();
    val s = e.getStackTrace()(2).toString
    printf("%s. Elapsed: %.3f sec\n", s, elapsed)
    ans
  }

}
