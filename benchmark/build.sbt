name := "benchmark"

Jmh / sourceDirectory := (Test / sourceDirectory).value
Jmh / classDirectory:= (Test / classDirectory).value
Jmh / dependencyClasspath := (Test / dependencyClasspath).value
  // rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile' (otherwise a clean 'jmh:run' would fail)
Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value
Jmh / run := (Jmh / run).dependsOn(Jmh / Keys.compile).evaluated