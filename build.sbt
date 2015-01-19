name := "tigerc"

version := "0.1"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq (
//                    "org.scala-sbt" % "command" % "0.12.0",
                    "com.github.nikita-volkov" % "sext" % "0.2.3",
//                    "org.scala-lang" % "scala-library" % "2.11.1",
//                    "org.scala-lang" % "scala-reflect" % "2.11.1",
                    "org.scalatest" % "scalatest_2.11" % "2.2.1",
                    "com.gilt" %% "handlebars-scala" % "2.0.1"
)

unmanagedSourceDirectories in Compile <++= baseDirectory { base =>
  Seq(
    base / "/src/main/parser"
  )
}
