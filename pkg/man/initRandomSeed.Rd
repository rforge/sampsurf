\name{initRandomSeed}
\alias{initRandomSeed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Initialize the Random Number Seed}

\description{This routine will initialize \code{\link{.Random.seed}} to the desired
  seed, continue the current seed, or if none exists, set via the clock time.
}

\usage{
initRandomSeed(startSeed = NA, runQuiet = TRUE, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{startSeed}{\code{NULL} or \code{NA}: default to current stream, unless
  \code{.Random.seed} does not exit, in which case the seed is set from the
  system clock time; otherwise, specify a scalar seed value start a new stream.}

  \item{runQuiet}{\code{TRUE}: no feedback; \code{FALSE}: show some results.}

  \item{\dots}{ Not used, for gobbling arguments only. }
}

\details{ The default uniform \acronym{RNG} in \R
is\dQuote{Mersenne-Twister} which produces a \sQuote{seed set} when
initialized. This seed set seems to be initialized to postion 624 so
that the next draw will change the seed set (and hence the position to
1). Subsequently it will take 624 more draws to change the seed set, to
a new one &c. So what is returned by default from this function is a
vector of length 626.  }

\value{
  The newly set seed or current seed invisibly.
}
\author{Jeffrey H. Gove}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{.Random.seed}}, \code{\link{set.seed}} }
\examples{
\dontrun{
initRandomSeed()
initRandomSeed(12)
rm(.Random.seed)
initRandomSeed(NULL)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
