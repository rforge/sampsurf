\name{clipStemsToTract}
\alias{clipStemsToTract}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Clip stems to lie within tract }

\description{
This function and associated methods will ultimately take a collection
of "\code{\linkS4class{Stem}}" subclass objects and check that
they all lie within the associated "\code{\linkS4class{Tract}}" subclass
object. In addition, if some objects lie either paritally or completely
outside the tract, the routine will return a new collection with only
those portions of the collection that lie completely within the
boundaries of the tract if desired. This function is especially helpful
when using either the \sQuote{mirage} or \sQuote{walkthrough} methods to
correct for boundary overlap.
}

\usage{
clipStemsToTract(stems, tract, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{stems}{ A collection of \dQuote{\code{Stem}} subclass objects
	(i.e., "\code{\linkS4class{downLogs}}" or
	"\code{\linkS4class{standingTrees}}"). }

  \item{tract}{ A \dQuote{\code{Tract}} subclass object. }

  \item{\dots}{ See methods for other possible arguments. }

}

\details{

It is possible to have a collection of stems where some
individuals do not lie completely within a desired tract or plot in
\pkg{sampSurf}. When using buffered tracts, it is fine if some
individuals intersect the internal buffer, as long as the respective
inclusion zones do not overlap the external boundary. There are
circumstances when it is advantageous to clip any tree or log that lies
outside the tract out of the population. In addition, logs can intersect
the tract boundary, and clipping the external part that is not within
the tract is also sometimes useful. This is especially true when using
either the mirage or walkthrough boundary correction methods.

One can invoke this function through the appropriate method and it will
remove any stem that lies completely outside the tract, and also clip
any logs to the tract boundary. If one desires only to check whether any
stems are offending in this way, that option is also available. For
\dQuote{\code{standingTree}} objects, the center location must lie
outside the tract. For \dQuote{\code{downLog}} objects, the entire
needle must lie outside the tract before the object is removed. If the
needle intersects the tract, the external portion is clipped
accordingly.
  
This generic function has several methods
\code{\link{clipStemsToTract-methods}}, which should be consulted for
more details on its use. Note that the method for buffered tracts clips to the
internal buffer by default, but one can alternatively choose to clip to
the tract boundary if desired.
}

\value{

  The data returned are in \code{list} form with components\ldots

  \item{df}{ A data frame with as many rows as there are stems in the
  collection \emph{originally passed} to the function. Columns identify
  which stems lie inside, outside and intersect, and if the latter, on
  which cardinal direction leg of the tract that occurs (can be more than
  one, e.g., in a corner).}

  \item{status}{ A simple numeric summary of stems lying inside, outside and
  intersecting.} 

  \item{stems}{ The new collection of stems (or \code{NA} if all stems
	are inside) with the offending stems either removed or clipped.}
}

\references{
%% ~put references to the literature/web site here ~
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\note{
  Please note that  only two of the
  \code{\link{clipStemsToTract-methods}} methods should actually be
  called by the casual user. The others simply set up the control flow
  for doing all the dirty work.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  "\code{\linkS4class{StemContainer}}", "\code{\linkS4class{Tract}}"
}

\examples{
#
# make a smaller "plot" inside the large one and clip...
#
\dontrun{
btr = bufferedTract(.1, Tract(c(x=50,y=50), cellSize=1))
bb = bbox(btr)
bb[,1] = 20
bb[,2] = 40
smtr = Tract(bb, cellSize=1)
dlogs = downLogs(100, btr)
dlogs2 = clipStemsToTract(dlogs, smtr, runQuiet=FALSE, showPlot=TRUE)
dlogs2$status
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
