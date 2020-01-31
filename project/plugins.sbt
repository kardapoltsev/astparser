resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  "Sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

addSbtPlugin("org.scoverage"          % "sbt-scoverage"  % "1.5.1")
addSbtPlugin("org.scoverage"          % "sbt-coveralls"  % "1.2.5")
addSbtPlugin("de.heikoseeberger"      % "sbt-header"     % "5.4.0")
addSbtPlugin("org.xerial.sbt"         % "sbt-sonatype"   % "3.8")
addSbtPlugin("com.jsuereth"           % "sbt-pgp"        % "2.0.1")
addSbtPlugin("com.github.gseitz"      % "sbt-release"    % "1.0.13")
addSbtPlugin("com.geirsson"           % "sbt-scalafmt"   % "1.5.1")
addSbtPlugin("ch.epfl.scala"          % "sbt-scalafix"   % "0.9.11")
addSbtPlugin("com.sksamuel.scapegoat" %% "sbt-scapegoat" % "1.0.9")
