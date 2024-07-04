import org.scalajs.linker.interface.ModuleSplitStyle

val scala3Version = "3.3.3"

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin) // Enable the Scala.js plugin in this project
  .settings(
    name                            := "catenary",
    version                         := "0.1.0-SNAPSHOT",
    scalaVersion                    := scala3Version,
    scalacOptions ++= Seq(
      "-language:strictEquality",
      "-source:future",
      "-feature",
      "-deprecation",
      "-Ykind-projector:underscores",
      "-Ysafe-init",
      "-Xmax-inlines:256",
      "-language:implicitConversions",
      "-Wunused:all",
      "-Wvalue-discard"
    ),
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("org.felher.catenary")))
    },
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom"   % "2.4.0",
      "com.raquo"    %%% "laminar"       % "17.0.0",
      "io.circe"     %%% "circe-core"    % "0.14.9",
      "io.circe"     %%% "circe-generic" % "0.14.9",
      "io.circe"     %%% "circe-parser"  % "0.14.9",
      "dev.optics"   %%% "monocle-macro" % "3.1.0"
    )
  )
