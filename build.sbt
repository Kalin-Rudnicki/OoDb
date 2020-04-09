
lazy val global = project
	.in(file("."))
	.aggregate(db)

lazy val db = project
	.in(file("db")).settings(
	libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.30"
)


name := "OoDb"
version := "0.1"
scalaVersion := "2.12.6"
