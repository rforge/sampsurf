\name{.StemEnv}
\alias{.StemEnv}
\docType{data}

\title{ \sQuote{Hidden} Environment for Constants, etc.}

\description{
  This environment is used throughout the \pkg{sampSurf} package. Even
  though it is hidden, it is simply an environment to put \sQuote{global}
  variables/parameters in an out of the way place. It is exported from
  the \pkg{sampSurf} namespace, so you can access any objects within it.
}

  
\author{
Jeffrey H. Gove
}


\examples{
require(sampSurf)
ls(.StemEnv)
.StemEnv$sampleLogsNames
get('wbTaper', envir=.StemEnv)
}
\keyword{datasets}
