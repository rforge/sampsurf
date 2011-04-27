\name{boltDimensions}
\alias{boltDimensions}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Calculate Bolt Dimensions for \dQuote{downLog} Objects}

\description{ This function will calculated volume, length, surface
  area, etc. for each bolt in a \dQuote{downLog} object, where the bolts are
  defined by the \code{taper} slot.
}

\usage{
boltDimensions(dlog, runQuiet = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dlog}{ An object of class \code{"\linkS4class{downLog}"} }
  
  \item{runQuiet}{ \code{TRUE}: no feedback; \code{FALSE}: print a summary report}
}

\details{
  Nothing is stored in a \dQuote{downLog} object with respect to the
  individual bolts except the taper measurements in the \code{taper}
  slot. This allows the easy computation of different attributes for
  each bolt as described above.
}

\value{
  A data frame with self-evident columns, and rows representing
  bolts. For example, cross-check the diameters and lengths with the
  log's \code{taper} slot. The "biomass" and "carbon" columns will be
  \code{NA} if no conversions have been given for the log at creation.
}


\author{
Jeffrey H. Gove
}

\seealso{\code{"\linkS4class{downLog}"}
}
\examples{
dl = downLog(logLen=5, nSegs=10)
dl@taper
(boltDimensions(dl))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
