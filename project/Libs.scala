import sbt.*

object Libs {

  val sttpVersion = "3.9.7"
  val zioVersion = "2.1.6"

  val libraryDependencies = Seq(
    "net.codingwell" %% "scala-guice" % "6.0.0",
    "com.softwaremill.sttp.client3" %% "core" % sttpVersion,
    "com.softwaremill.sttp.client3" %% "okhttp-backend" % sttpVersion,
    "org.slf4j" % "slf4j-api" % "2.0.5",
    "org.typelevel" %% "cats-core" % "2.9.0",
    "dev.zio" %% "zio" % zioVersion,
    "dev.zio" %% "zio-cache" % "0.2.3",
    "dev.zio" %% "zio-test" % zioVersion,
    "dev.zio" %% "zio-test-sbt" % zioVersion,
    "dev.zio" %% "zio-streams" % zioVersion,
    "dev.zio" %% "zio-interop-cats" % "23.0.03",
    "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion,
    "org.scalatest" %% "scalatest" % "3.2.15" % Test,
    "org.scalatest" %% "scalatest-wordspec" % "3.2.16" % Test,
  )
}