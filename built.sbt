//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4"

//libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.6"

//libraryDependencies += "jgrapht" % "jgrapht-jdk1.6" % "0.8.3" from "http://ccl.northwestern.edu/devel/jgrapht-jdk1.6-0.8.3.jar"
libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.0.0"

//libraryDependencies += "jdd" % "jdd" % "105" from "https://bitbucket.org/vahidi/jdd/downloads/jdd_105.jar"
libraryDependencies += "de.fosd.typechef" % "javabdd_repackaged" % "1.0b2"

//lazy val jbdd = ProjectRef(uri("git://github.com/martin-riedl/JavaBDD_repackaged#master"), "javabdd_repackaged") 
//scalaVersion in jbdd := "2.11.8"
 
lazy val commonSettings = Seq(
  organization := "ftse",
  version := "1.0.0", 
  scalaVersion := "2.11.8"
)

lazy val base = ProjectRef(file("../base/"),"base")

lazy val lares = (project in file(".")).
  dependsOn(base).
  settings(commonSettings: _*). 
  settings(
    name := "lares"
  ) 

