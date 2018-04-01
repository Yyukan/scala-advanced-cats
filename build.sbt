name := "Scala Advanced Cats"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.0"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-Xfatal-warnings",
  "-Ypartial-unification"
)

