\name{sampleTrees}
\alias{sampleTrees}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Generates a Synthetic Population of Standing Tree Attributes }

\description{ This routine will allow the generation of a collection of
  synthetic standing tree \emph{attributes} in a
  \code{\link{data.frame}}. Note that the function returns a collection
  of tree attributes in the sense of dimensions, etc., and \emph{not} a
  collection of \code{\linkS4class{standingTree}} objects, although it can be
  used to generate one if desired (see examples). Please see \dQuote{The
  Stem Class} vignette for more details.
}

\usage{
sampleTrees(nTrees = 2,
            dbhs = c(8, 40),
            topDiams = c(0.4, 0.9),
            heights = c(5, 15),
            solidTypes = c(1, 10),
            species = .StemEnv$species,
            sampleRect = NULL,
            startSeed = NA,
            runQuiet = FALSE,
            ...)
}

\arguments{
  \item{nTrees}{ The number of trees in the collection.}
  
  \item{dbhs}{ A length-two vector specifying the \emph{range} of
        breast height diameters from which to uniformly draw the
        sample.}

  \item{topDiams}{ A length-two vector specifying the \emph{range} of
        top diameters, in the form of a \emph{proportion} of
        the \code{dbh} diameters, from which to uniformly draw the
        sample. }

  \item{heights}{ A length-two vector specifying the \emph{range} of tree
        heights from which to uniformly draw the sample.}

  \item{solidTypes}{ A length-two vector specifying the \emph{range} in
        the tree shape parameter for the default taper and volume
        equations; where: 1-2 is a neiloid, 2 is a cone, and >2 is a
        paraboloid. }

  \item{species}{ A character vector of possible species from which
        to draw the sample uniformly. This can be any legal character
        string, and so can include codes, names, Latin names, etc. }

  \item{sampleRect}{ A rectangle delineating the area within which the
	   tree centers (pith at dbh) will be sampled from. It must be in
	   the form of a valid \code{\link{bbox}} matrix. If \code{NULL} or
	   \code{NA}, a unit square bounding box will be created. }

  \item{startSeed}{ A numeric start seed for the random number
	   generator. \code{NA} will continue with the current random number
	   stream if available, or create one if not. }

  \item{runQuiet}{ \code{TRUE}: No feedback; \code{FALSE}: a little
    feedback. }
  
  \item{\dots}{ Not used at present, just gobbles up other arguments. }
}

\details{ This routine does not care about units, it is up to the
  user to make sure that the ranges for diameters and heights are
  reasonable for whatever system (i.e., "English" or "metric") one is
  working in. For example, if metric units are used, \code{dbhs}
  would be in centimeters, while \code{heights} would be in meters.
}

\value{ A data frame with columns: \code{species, height, dbh, topDiam,
  solidType, x, y} where \code{x,y} is the center point of the tree pith
  at \code{dbh}.  }


\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
    \code{\link{standingTrees-methods}}
}

\examples{
#
#  draw from a unit square (metric default)...
#
sampleTrees(species=c('ewp','sm','978'),dbhs=c(4,10), heights=c(3,6))
#
# draw from a buffer...
#
bufftr = bufferedTract(10, Tract(c(x=50,y=50),cellSize=0.5))
st = sampleTrees(10, sampleRect=bufftr@bufferRect, dbhs=c(20,40))
strees = standingTrees(st)
plot(bufftr, axes=TRUE, gridColor='grey80')
plot(strees, add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
