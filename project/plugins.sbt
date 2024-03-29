resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  "Sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

addSbtPlugin("org.scoverage"           % "sbt-scoverage" % "1.5.1")
addSbtPlugin("org.scoverage"           % "sbt-coveralls" % "1.2.5")
addSbtPlugin("de.heikoseeberger"       % "sbt-header"    % "5.4.0")
addSbtPlugin("org.xerial.sbt"          % "sbt-sonatype"  % "3.8")
addSbtPlugin("com.jsuereth"            % "sbt-pgp"       % "2.0.1")
addSbtPlugin("com.github.sbt"          % "sbt-release"   % "1.1.0")
addSbtPlugin("ch.epfl.scala"           % "sbt-scalafix"  % "0.11.0")
addSbtPlugin("org.scalameta"           % "sbt-scalafmt"  % "2.4.6")
