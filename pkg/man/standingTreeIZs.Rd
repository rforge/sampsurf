\name{standingTreeIZs}
\alias{standingTreeIZs}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{standingTreeIZs}}"
}

\description{ This generic function has one method based on the
  signature formal argument \code{object}. It is used as a constructor
  function for objects that are of class
  "\code{\linkS4class{standingTreeIZs}}". The associated method should be
  used in preference to \code{\link{new}} to insure a valid object.  }

\usage{
standingTreeIZs(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{object}{ Signature formal argument. }

  \item{\dots}{ Formal arguments that are different for each method, see
	those for details. }
}

\details{
The methods that can be used to generate objects of class
\dQuote{\code{\linkS4class{standingTreeIZs}}} are detailed here:
\code{\link{standingTreeIZs-methods}}.}

\value{
A valid object of class "\code{\linkS4class{standingTreeIZs}}."
}

\references{
\dQuote{\emph{The InclusionZone Class}} vignette.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  For subclasses of \code{\linkS4class{standingTreeIZ}}, see:
  \code{\linkS4class{circularPlotIZ}},
}

\examples{
sts = standingTrees(4, units='English', heights=c(10,30))
sts.cp = lapply(sts@trees, 'circularPlotIZ', plotRadius=20)
izs.cp = standingTreeIZs(sts.cp)
bbox(izs.cp)
plot(izs.cp, axes=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
