\name{gridCellEnhance}
\alias{gridCellEnhance}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Add Grid Lines and Centers in Package \dQuote{sampSurf} }

\description{ This routine is found within several of the plotting
  methods for objects of class "\code{\linkS4class{Tract}}" or it
  subclasses. In these methods, the arguments detailed below are passed
  as part of the "\dots" list. It can, however, also be used as a
  stand-alone function if desired, and applied to an existing plot of
  \dQuote{Tract} objects, or objects that contain a \dQuote{Tract}
  object, such as those of class
  "\code{\linkS4class{sampSurf}}". Lastly, it is used directly in
  plotting objects of class "\code{\linkS4class{InclusionZoneGrid}}" of
  subclasses. }

\usage{
gridCellEnhance(tract,
                gridLines = FALSE,
                gridCenters = FALSE,
                gridLineColor = .StemEnv$gridLineColor,
                gridCenterColor = .StemEnv$gridCenterColor,
                lwdGrid = 1, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tract}{ An object of class \dQuote{Tract} or subclass. }

  \item{gridLines}{ \code{TRUE}: plot grid lines; \code{FALSE}: no grid lines.}

  \item{gridCenters}{ \code{TRUE}: plot grid cell centers; \code{FALSE}:
	no grid cell centers. }

  \item{gridLineColor}{ An \R color for the grid lines.}

  \item{gridCenterColor}{ An \R color for the grid cell centers. }

  \item{lwdGrid}{ The line width specification in the form of the
	\code{lwd} \code{\link{par}} parameter, for the grid lines.}

  \item{\dots}{ Arguments to be ignored at present. }
}

\details{ A valid plot must have been constructed first for the
  \dQuote{Tract} object as the embellishments will be added to it. This
  should probably only be used on small areas, since the lines or
  centers will overlap for large areas or high resolution objects.
}

\value{ Invisibly.}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{ \code{\link{plot-methods}} }

\examples{
#
# apply it after plotting a sampling surface...
#
tr = Tract(c(x=50,y=50), cellSize=0.5)
btr = bufferedTract(10, tr)
ssSA = sampSurf(2, btr, iZone = 'sausageIZ', plotRadius=3,
       buttDiam=c(20,40))
plot(ssSA, axes=TRUE)
gridCellEnhance(ssSA@tract, gridLines=TRUE)

#
# on a tract only...
#
plot(btr, axes=TRUE, gridCenters=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
