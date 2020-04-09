
lazy val global = project
	.in(file("."))
	.aggregate(db)

lazy val db = project
	.in(file("db")).settings(
	libraryDependencies ++= List(
		"org.scalaz" %% "scalaz-core" % "7.2.30",
		"org.rogach" %% "scallop" % "3.4.0"
		)
)


name := "OoDb"
version := "0.1"
scalaVersion := "2.12.6"
