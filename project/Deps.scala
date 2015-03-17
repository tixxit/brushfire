import sbt._

object Resolvers {
  val conjars = "Conjars Repo" at "http://conjars.org/repo"
}

object Deps {
  object V {
    val algebird = "0.9.0"
    val jackson = "1.9.2"
    val bijection = "0.7.0"

    val hadoopClient = "2.5.2"
    val scalding = "0.13.1"
    val chill = "0.5.2"

    val finatra = "1.6.0"
    val storehaus = "0.10.0"
    val aws = "1.9.23"
  }

  val algebirdCore   = "com.twitter"         %% "algebird-core"      % V.algebird
  val bijectionJson  = "com.twitter"         %% "bijection-json"     % V.bijection
  val chillBijection = "com.twitter"         %% "chill-bijection"    % V.chill
  val jacksonMapper  = "org.codehaus.jackson" % "jackson-mapper-asl" % V.jackson
  val jacksonXC      = "org.codehaus.jackson" % "jackson-xc"         % V.jackson
  val jacksonJAXRS   = "org.codehaus.jackson" % "jackson-jaxrs"      % V.jackson

  val hadoopClient   = "org.apache.hadoop"    % "hadoop-client"      % V.hadoopClient
  val scaldingCore   = "com.twitter"         %% "scalding-core"      % V.scalding

  val finatra        = "com.twitter"         %% "finatra"            % V.finatra
  val s3             = "com.amazonaws"        % "aws-java-sdk-s3"    % V.aws
  val storehaus      = "com.twitter"         %% "storehaus-core"     % V.storehaus
}
