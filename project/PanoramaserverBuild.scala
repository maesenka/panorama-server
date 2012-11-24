import sbt._
import sbt.Keys._

object PanoramaserverBuild extends Build {

  lazy val panoramaserver = Project(
    id = "panorama-server",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "panorama-server",
      organization := "org.geolatte",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.2"
      // add other settings here
    )
  )
}
