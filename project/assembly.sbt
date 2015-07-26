resolvers += Resolver.url(
   "bintray-sbt-plugin-releases",
     url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
         Resolver.ivyStylePatterns)
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.13.0")
