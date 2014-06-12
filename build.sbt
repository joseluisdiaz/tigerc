name := "tigerc"

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq ( 
//                    "org.scala-sbt" % "command" % "0.12.0",
                    "com.github.nikita-volkov" % "sext" % "0.2.3",
                    "org.scalatest" % "scalatest_2.10" % "2.0.M8"
)

unmanagedSourceDirectories in Compile <++= baseDirectory { base =>
  Seq(
    base / "/src/main/parser"
  )
}
