ThisBuild / scalaVersion     := "3.3.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "adventOfCode24",
    libraryDependencies ++= Libs.libraryDependencies,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
