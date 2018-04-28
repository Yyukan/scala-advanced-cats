name := "Scala Advanced Cats"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.1"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-Xfatal-warnings",
  "-Ypartial-unification",
  "-language:higherKinds"
)

