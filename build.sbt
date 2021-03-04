
lazy val Cats      = "org.typelevel" %% "cats-core" % "2.1.1"
lazy val ScalaTest = "org.scalatest" %% "scalatest" % "3.2.5" % "test"
lazy val app = (project in file(".")).settings(
  name := "cinvestav-is-elgammal",
  version := "0.1",
  scalaVersion := "2.13.5",
  libraryDependencies ++= Seq(Cats,ScalaTest)
)
