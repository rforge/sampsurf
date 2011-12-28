\name{downLog-methods}
\docType{methods}
\alias{downLog-methods}
\alias{downLog,missing-method}
\alias{downLog,data.frame-method}

\title{  Methods for "downLog" Object Construction}
\description{
 There are currently two available methods for the generic
 \code{\link{downLog}}. These methods generate objects of class
 "\code{\linkS4class{downLog}}" that are valid objects. This is the
 preferred method for generating such objects, rather than using
 \code{\link{new}}.
}

\section{Methods}{
\describe{

  \item{\strong{\code{signature(object = "missing")}}}{%\cr

	This constructor will be used when the signature argument is
    "missing."  When \code{object} is \code{\link{missing}}, the taper
    data is generated from the internal taper function between the
    \code{buttDiam} and \code{topDiam} diameters specified, in
    \code{nSegs} sections. The taper function used is documented in
    \emph{\dQuote{The Stem Class}} vignette and references for
    "\code{\linkS4class{downLog}}." The following arguments are part of
    the function call; all arguments with the same names as class slots
    are also defined in the class definition (and may be stored
    differently than the arguments).}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(
         buttDiam = 5,                 #cm
         topDiam = 0,                  #cm
         logLen = 5,                   #meters
         nSegs = 20,
         solidType = 3,                #defaults to 3  
         logAngle = 0,                 #canonical position
         logVol = NULL,
         surfaceArea = NULL,
         coverageArea = NULL,
         biomass = NA,
         vol2wgt = NA,
         carbon = NA,
         wgt2carbon = NA,
         centerOffset = c(x=0, y=0),   #log center offset
         species = '',
         logID = paste('log',format(runif(1),digits=6),sep=':'),
         description = NULL,
         userExtra = NULL,
         units = 'metric',
         spUnits = CRS(projargs=as.character(NA)),
         runQuiet = TRUE,
         ...)}
	   
    \itemize{
	  
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
        \code{NULL}, the log volume will be computed from the taper volume
        equation or Smalian's formula if \code{solidType=NULL}. }
	  
	  \item{\code{surfaceArea}:}{ The log surface area if precomputed, otherwise, if
        \code{NULL}, the log surface area will be computed from the taper
        equation or spline approximation. }
	  
	  \item{\code{coverageArea}:}{ The log coverage area if precomputed, otherwise, if
        \code{NULL}, the log coverage area will be computed from the taper
        equation or spline approximation. }
	  
	  \item{\code{biomass}:}{ The log woody biomass if precomputed,
        otherwise, if \code{NA}, the log biomass will be computed from
        the volume and \code{vol2wgt} conversion. }
	  
	  \item{\code{vol2wgt}:}{ The volume to weight conversion factor. If
	  \code{NA} and \code{biomass} is passed, then it will be computed.}
	  
	  \item{\code{carbon}:}{ The log carbon content if precomputed, otherwise, if
        \code{NA}, the carbon content will be computed from the biomass and
        \code{wgt2carbon} conversion. }
	  
	  \item{\code{wgt2carbon}:}{ The weight to carbon conversion factor. If
	  \code{NA} and \code{carbon} is passed, then it will be computed.}
		
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

	  \item{\code{units}:}{ Either \dQuote{English} or \dQuote{metric}. These
	    must be conformable with the projection in \code{spUnits}.}
	  
	  \item{\code{spUnits}:}{ A valid \code{\link{CRS}} object
        specifying the Coordinate Reference System. This defaults to
        \code{NA}, which means you want to use your own user-defined
        system, say for a sample plot where the log has been located in
        the field.}
	  
	  \item{\code{runQuiet}:}{ \code{TRUE}: no feedback during object creation;
        \code{FALSE}: prints some information along the way.}
	  
	  \item{... :}{ Other arguments to be passed along.}
	  
	} %itemize
  } %item


  \item{\strong{\code{signature(object = "data.frame")}}}{%\cr

	When \code{object} is a "data.frame," then it is assumed that the
    data frame contains the taper data in the form of diameters and
    lengths (as columns with labels "diameter" and "length",
    respectively), with diameters in the \emph{same} units as
    length. All arguments except those listed below are the same as in
    the previous constructor\ldots}
  
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(object
         solidType = NULL,             #defaults to null for passed taper
         logAngle = 0,                 #canonical
         logVol = NULL,
         surfaceArea = NULL,
         coverageArea = NULL,
         biomass = NA,
         vol2wgt = NA,
         carbon = NA,
         wgt2carbon = NA,
         centerOffset = c(x=0, y=0),   #log center offset
         species = '',
         logID = paste('log',format(runif(1),digits=6),sep=':'),
         description = NULL,
         userExtra = NULL,
         units = 'metric',
         spUnits = CRS(projargs=as.character(NA)),
         runQuiet = TRUE,
         ...)}
	   
    \itemize{
		 
	  \item{\code{object}:}{ A data frame (see note below).}
	  
	  \item{\code{solidType}:}{ \code{NULL} is the default, because it
        is assumed that the taper data are from field measurements or
        have been generated from a different taper equation where this
        would not be a meaningful parameter. One can therefore have an
        educated guess about the genesis of taper data within an object
        by querying this slot in the completed object.}
	  
	  \item{\code{logVol}:}{ The log volume if precomputed, otherwise, if
        \code{NULL}, the log volume will be computed from the taper
        data frame using Smalian's method. }

	  
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
