\name{checkStemDimensions}
\alias{checkStemDimensions}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Check for consistency in attribute slots within \dQuote{Stem} Objects
}

\description{
  Stem class constructors allow one to specify both taper and total stem
  attributes such as volume and biomass. This function checks to make
  sure that any inconsistencies between the taper version of these
  attributes and those specified in the constructor are not too
  large. See below for details.
}

\usage{
checkStemDimensions(stem, tolerancePercent = 1, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{stem}{ An object that is a subclass of
	\code{"\linkS4class{Stem}"} }

  \item{tolerancePercent}{ The tolerance limit in percent (the default
  is 1 percent). Differences larger than this will result in a
  warning. }

  \item{\dots}{ Just gobbled for now.}

}


\details{
  If one specifies a volume (optional) for a \dQuote{Stem} object in the
  constructor in addition to the taper data (required), the two may not
  necessarily be in agreement. The same is true for other attibutes like
  surface area, etc. This is okay for the usual Horvitz-Thompson type
  estimators where the sampling surface is flat (constant) within a
  given inclusion zone. The routines use the entered volume for
  estimation and the taper data for graphical display. However, the
  Monte Carlo sampling methods (such as distance limited sampling for
  down logs, or critical height sampling for standing trees) use the
  taper data to determine heights or diameters along the bole as part of
  the estimate. Thus, if the two values for volume (that in the volume
  slot and that given by integration of the taper data) are not closely
  commensurate, it can result in an unintended apparent bias in the
  simulations. Note also that methods like perpendicular distance
  sampling rely on the taper data for the inclusion zone, so there could
  be a bias produced here as well.

  This function can be used to check how closely the slot and taper
  values correspond for a given \dQuote{Stem} subclass object, and will
  print a warning if the discrepancy is too large. One should not ignore
  this warning in the cases mentioned above. More information on this is
  given in the reference below.
}

\value{
A \code{list} is returned invisibly with\ldots

\item{stemID}{ The stem ID value.}

\item{tolerancePercent}{ The value of the above argument specified by
  the user.}

\item{volume}{ A vector with elements: 1. volume from the slot,
 2. volume from either the taper function or Smalian's, depending on solidType,
 3. splined volume from the taper data, 4. the percent difference. }

\item{surfaceArea}{ A vector with elements: 1. surface area slot,
  2. splined or taper SA depending on solidType, 3. percent difference.}

\item{coverageArea}{ As above in \code{surfaceArea} but for coverage
  area.}

\item{biomass}{ A vector with elements: 1. biomass slot value, 2. conversion
  from \code{volume[2]}, 3. percent difference. Note that if no
   biomass is specified in the respective object slot, all will be
   \code{NA}. }

\item{carbon}{ As above for \code{biomass} but for carbon.} 

} %value


\references{
\describe{
%
\item{}{Gove, J. H. 2014. Apparent bias in \pkg{sampSurf}. \emph{\R
	\pkg{sampSurf} package vignette series paper.} }

}%describe
}

\author{
Jeffrey H. Gove
}

\note{ Note that as of \pkg{sampSurf} version 0.7-2 this function is run
  automatically within the constructors for down logs or standing
  trees. However, one can run it at any time as a check on the object,
  just like one can invoke \code{\link{validObject}} at any time.  }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{"\linkS4class{Stem}"}, \code{"\linkS4class{downLog}"}, \code{"\linkS4class{standingTree}"}
}

\examples{
\dontrun{
st = standingTree(dbh=20, height=20, solidType=3)
checkStemDimensions(st)
st2 = standingTree(dbh=20, height=20, solidType=3, treeVol=1.1*st@treeVol)
checkStemDimensions(st2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
