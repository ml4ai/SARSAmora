name := "Toy Domains"

javaOptions in run += "-Djava.library.path=/usr/local/lib/python2.7/site-packages/jep"
libraryDependencies += "me.shadaj" %% "scalapy" % "0.1.0-SNAPSHOT"