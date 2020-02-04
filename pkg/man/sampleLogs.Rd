\name{sampleLogs}
\alias{sampleLogs}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Generates a Synthetic Population of Down Log Attributes }


\description{ This routine will allow the generation of a collection of
  synthetic down log \emph{attributes} in a
  \code{\link{data.frame}}. Note that the function returns a collection
  of log attributes in the sense of dimensions, etc., and \emph{not} a
  collection of \code{\linkS4class{downLog}} objects, although it can be
  used to generate one if desired (see examples). Please see \dQuote{The
  Stem Class} vignette for more details.  }

\usage{
sampleLogs(nLogs = 2,
           buttDiams = c(8, 40),
           topDiams = c(0, 0.9),
           logLens = c(1, 10),
           logAngles = c(0, 2 * pi),
           solidTypes = c(1, 10),
           species = .StemEnv$species,
           sampleRect = NULL,
           startSeed = NA,
           runQuiet = FALSE,
           ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nLogs}{ The number of logs in the collection. }
  
  \item{buttDiams}{ A length-two vector specifying the \emph{range} of
        butt (large-end) diameters from which to uniformly draw the
        sample. }

  \item{topDiams}{ A length-two vector specifying the \emph{range} of
        top (small-end) diameters, in the form of a \emph{proportion} of
        the \code{buttDiam} diameters, from which to uniformly draw the
        sample.  }
  
  \item{logLens}{ A length-two vector specifying the \emph{range} of log
        lengths from which to uniformly draw the sample. }

  \item{logAngles}{ A length-two vector specifying the \emph{range}
        of log angles from which to draw the log lie angles; these are
        always counterclockwise relative to due East.}

  \item{solidTypes}{ A length-two vector specifying the \emph{range} in
        the log shape parameter for the default taper and volume
        equations; where: 1-2 is a neiloid, 2 is a cone, and >2 is a
        paraboloid. }

  \item{species}{ A character vector of possible species from which
        to draw the sample uniformly. This can be any legal character
        string, and so can include codes, names, Latin names, etc.  }
  
  \item{sampleRect}{ A rectangle delineating the area within which the
	log centers will be sampled from. It must be in the form of a valid
	\code{\link{bbox}} matrix. If \code{NULL} or \code{NA}, a unit square
	bounding box will be created.}

  \item{startSeed}{ A numeric start seed for the random number
	generator. \code{NA} will continue with the current random number
	stream if available, or create one if not. }
  
  \item{runQuiet}{ \code{TRUE}: No feedback; \code{FALSE}: a little feedback. }
  
  \item{\dots}{ Not used at present, just gobbles up other arguments.}
}

\details{ This routine does not care about units, it is up to the
  user to make sure that the ranges for diameters and lengths are
  reasonable for whatever system (i.e., "English" or "metric") one is
  working in. For example, if metric units are used, \code{buttDiams}
  would be in centimeters, while \code{logLens} would be in meters.
}

\value{
  A data frame with columns: \code{species, logLen, buttDiam, topDiam,
  solidType, x, y, logAngle, logAngle.D} where \code{x,y} is the center
  point of the log and \code{logAngle.D} is in degrees.
}



\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  \code{\link{downLogs-methods}}
}

\examples{
#
#  draw from a unit square...
#
sampleLogs(species=c('ewp','sm','978'),buttDiams=c(1,10), logLens=c(3,6))
#
# draw from a buffer...
#
bufftr = bufferedTract(10, Tract(c(x=50,y=50),cellSize=0.5))
sl = sampleLogs(10, sampleRect=bufftr@bufferRect, buttDiams=c(25,40))
dlogs = downLogs(sl)
plot(bufftr, axes=TRUE, gridColor='grey80')
plot(dlogs, add=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
