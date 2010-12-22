\name{downLog-methods}
\docType{methods}
\alias{downLog-methods}
\alias{downLog,missDF-method}

\title{  Methods for "downLog" Object Construction}
\description{
 There is currently only one available method for the generic
 \code{\link{downLog}}. This method generates objects of class
 "\code{\linkS4class{downLog}}" that are legal objects. This is the
 preferred method for generating such objects instead of using
 \code{\link{new}}. Other constructors could be added by extention to this
 primary method.
}

\section{Methods}{
\describe{

  \item{\code{signature(object = "missDF")}}{\cr The "missDF" class is a
    class union of "data.frame" and "missing." If \code{object} is a
    data.frame, then it is assumed that the data frame contains the taper
    data in the form of diameters and lengths (as columns with labels
    "diameter" and "length", respectively), with diameters in the
    \emph{same} units as length. If object is \code{\link{missing}},
    the taper data is generated from the internal taper function between
    the \code{buttDiam} and \code{topDiam} diameters specified, in
    \code{nSegs} sections. The taper function used is documented in \emph{\dQuote{The
    Stem Class}} vignette and references for "\code{\linkS4class{downLog}}." The
    following arguments are part of the function call; all arguments
    with the same names as class slots are defined in the class
    definition.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(object,
         buttDiam = 5,
         topDiam = 0,
         logLen = 5,
         nSegs = 20,
         solidType = NULL,
         logAngle = 0,
         logVol = NULL,
         centerOffset = c(x=0, y=0),
         species = '',
         logID = paste('log',format(runif(1),digits=5),sep=':'),
         description = NULL,
         userExtra = NULL,
         units = 'metric',
         spUnits = CRS(projargs=as.character(NA)),        % wantPlot = FALSE,
         runQuiet = FALSE,
         ...)}
	   
    \itemize{
		 
	  \item{\code{object}:}{ A data frame (see note below) or missing as
        described above keying on taper generation.}
	  
      \item{\code{buttDiam}:}{ The large-end diameter. For object
        creation, units are in either \emph{inches} or
        \emph{cm}. Internally, within the object, they are stored in the
        same units as length: \emph{feet} or \emph{meters}, depending on
        the value for \code{units}.}
	  
      \item{\code{topDiam}:}{ The small-end diameter with same units as
        large-end.}
	  
	  \item{\code{logLen}:}{ The log length in meters or feet, depending
        on \code{units}.}
	  
	  \item{\code{nSegs}:}{ The number of log segements to be generated
		from the taper function for the "missing" signature. Note that
        there will be \code{nSegs+1} diameter measurements for the log
		taper.}
	  
	  \item{\code{solidType}:}{ The type of solid for the default taper
        equation; the range is from 1 to 10, with 1 being a neiloid, 2 a
        cone and 3 a paraboloid. \code{NULL} is the default, when the
        \code{object} argument is missing it defaults to a value of 3.}
	  
	  \item{\code{logAngle}:}{ The log lie angle specified from the
        center of the log. It can take a value from 0 to \code{2*pi} and
        is releative to East being zero.}
	  
	  \item{\code{logVol}:}{ The log volume if precomputed, otherwise, if
        \code{NULL}, the log volume will be computed from the taper
        volume equation. }
	  
	  \item{\code{centerOffset}:}{ The log center position that will be
        used for the \code{location} slot. This is a vector of length two
        with names "x" and "y"; note that it can be length three with a "z"
        coordinate, but it is not used anywhere currently.}
	  
	  \item{\code{species}:}{ Some species identifier as a
        character string.}
	  
	  \item{\code{logID}:}{ Each log should have its own \emph{unique}
        identifier that is used in constructing the \code{\link{Polygons}}
        object for the perimeter. This becomes very important when combining
        individual logs into a population or collection via the container
        class "\code{\linkS4class{downLogs}}." If nothing is supplied, a
        random ID is generated.}
	  
	  \item{\code{description}:}{ A character vector description of the
		log.}
	  
	  \item{\code{userExtra}:}{ Anything else that one wants to carry
        along with the log.}
	  
	  \item{\code{spUnits}:}{ A valid \code{\link{CRS}} object
        specifying the Coordinate Reference System. This defaults to
        \code{NA}, which means you want to use your own user-defined
        system, say for a sample plot where the log has been located in
        the field.}
	  
	  %\item{\code{wantPlot}:}{ \code{TRUE}: show the graphics plot; \code{FALSE}: just
	  %		return the object.}
	  
	  \item{\code{runQuiet}:}{ \code{TRUE}: no feedback during object creation;
        \code{FALSE}: prints some information along the way.}
	  
	} %itemize
  } %item
	   
  
  } %describe
} %section

\note{
  It may not be immediately apparent how the taper data in the data
  frame is to be structured, if you have this data available either from
  measurements, or from a different taper equation. The best way to
  check this out is to simply create a dummy "downLog" object and then
  \code{show} or \code{print} it for a summary, which will show the
  first few records of the structure. If that is not enough, then you
  can look at the structure with the \code{\link{@}} operator applied to
  the object's \code{taper} slot. Please also see the vignette mentioned
  above. Remember, the diameters in the taper data frame are expected to
  be in the same units as length for a data frame.
} %note

\author{Jeffrey H. Gove}

\seealso{
The "\code{\linkS4class{downLog}}" class and the \code{\link{downLog}} generic.
}

\examples{
#create a downLog object and show it
dl = downLog(buttDiam=20, solidType=2.1, logAngle=pi/3)
summary(dl)
}

\keyword{methods}
