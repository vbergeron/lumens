ThisBuild / scalaVersion := "3.7.3"

lazy val `lumens-core` = (project in file("modules/core"))
    .settings(
      libraryDependencies ++= Seq(
        "org.apache.lucene" % "lucene-core" % "10.3.2",
        "org.typelevel"    %% "cats-effect" % "3.6.3"
      )
    )
