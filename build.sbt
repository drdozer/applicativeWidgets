
resolvers += Resolver.bintrayRepo("tek", "maven")

addCompilerPlugin("tryp" %% "splain" % "0.1.2")

name := "applicativeWidgets"

version := "0.0.1"

scalaVersion := "2.12.1"

//scalaVersion := "2.11.8"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.8"

scalacOptions := Seq("-P:splain:implicits:true")