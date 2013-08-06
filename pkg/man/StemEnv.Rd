\name{.StemEnv}
\alias{.StemEnv}
\docType{data}

\title{ \sQuote{Hidden} Environment for Constants, etc.}

\description{ This environment is used throughout the \pkg{sampSurf}
  package. Even though it is hidden, it is simply an environment to hold
  \sQuote{global} variables/parameters and functions in a central, but
  \dQuote{out of the way} place. It is exported from the \pkg{sampSurf}
  namespace, so you can access any objects within it, though most of the
  contents will not be of interest to the majority of users. Also note
  that it has been locked for safety, so one can not add or change its
  contents.

  Please see the code itself for information on the contents of this
  environment and the purpose of each object it contains.

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
