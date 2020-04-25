
lazy val global = project
	.in(file("."))
	.aggregate(db)

lazy val db = project
	.in(file("db")).settings(
	libraryDependencies ++= List(
		"org.scalaz" %% "scalaz-core" % "7.2.30",
		"org.rogach" %% "scallop" % "3.4.0",
		"org.scalactic" %% "scalactic" % "3.1.1",
		"org.scalatest" %% "scalatest" % "3.1.1" % "test"
		),
	ThisBuild / watchBeforeCommand := Watch.clearScreen
)


name := "SpyreDb"
version := "0.1"
scalaVersion := "2.12.6"
