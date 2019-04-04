name := "SelX"

     version := "0.1.0"
scalaVersion := "2.12.8"

enablePlugins(JmhPlugin)

sourceDirectory     in Jmh := (sourceDirectory                   in Test).value
classDirectory      in Jmh := (classDirectory                    in Test).value
dependencyClasspath in Jmh := (dependencyClasspath               in Test).value
compile             in Jmh := (compile in Jmh).dependsOn(compile in Test).value // rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile' (otherwise a clean 'jmh:run' would fail)
run                 in Jmh := (run     in Jmh).dependsOn(Keys.compile in Jmh).evaluated

libraryDependencies ++= Seq(
  "com.lihaoyi"  %% "utest"          % "0.6.6"  % "test",
  "org.scalanlp" %% "breeze"         % "0.13.2" % "test",
  "org.scalanlp" %% "breeze-natives" % "0.13.2" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")

scalacOptions ++= Seq(
//  "-opt:l:inline",
//  "-opt-inline-from:**"
)
