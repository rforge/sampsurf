\name{downLog-class}
\Rdversion{1.1}
\docType{class}
\alias{downLog-class}
\alias{bbox,downLog-method}
%\alias{chainSawIZ,downLog,numeric-method}
%\alias{plot,downLog,missing-method}
%\alias{sausageIZ,downLog,numeric-method}
%\alias{standUpIZ,downLog,numeric-method}
\alias{summary,downLog-method}

\title{Class "downLog": Representation of Downed Logs}

\description{
  A subclass of virtual class \dQuote{\code{\linkS4class{Stem}}} that can be used to
  represent downed coarse woody debris in the form of something
  resembling a log. The class provides for creation and graphical
  display of \dQuote{downLog} objects.

  Detail examples and information concerning this class is found in
  \dQuote{The Stem Class} vignette, which should be consulted for more
  details on creating and using objects from this class.
}

\section{Objects from the Class}{
  
Objects can be created by calls of the form \code{new("downLog", ...)}.
However, this is \emph{not} recommended. The object has a number of
required slots that can be somewhat complex to calculate. Therefore, a
constructor function \code{\link{downLog}} has been provided that will
make the desired object with the correct values for the slots. It is
strongly recommended that one use this generic's methods to create the
log objects.  }

\section{Slots}{

  Please note that all diameters below are presumed to be in the
  \emph{same} units as length, i.e., meters for \dQuote{metric}, and
  feet for \dQuote{English} \code{units}.

  In addition to the slots provided by the virtual superclass
  \dQuote{\code{\linkS4class{Stem}}}, the following slots are
  represented\ldots
  
  \describe{
	
    \item{\code{buttDiam}:}{Object of class \code{"numeric"}: the log
	  diameter at the large (butt) end in the proper units. }
	
    \item{\code{topDiam}:}{Object of class \code{"numeric"}: The
	  small-end diameter of the log. }
	
    \item{\code{logLen}:}{Object of class \code{"numeric"}: The log
	  length in meters or feet. }
	
    \item{\code{logAngle}:}{Object of class \code{"numeric"}: An angle
	  for the log's position established from the center of the log
	  relative to its "needle." The center of the log is equidistant
	  from both ends along the established needle. On a straight log,
	  the needle would correspond to the pith if everything were
	  perfect. }
	
    \item{\code{solidType}:}{Object of class
	  \code{"\linkS4class{numericNULL}"}: The form parameter in the
	  simple taper and volume equation presented in Van Deusen (1990)
	  and Gove and Van Deusen (2011).}
	
    \item{\code{logVol}:}{Object of class \code{"numeric"}: The log's
      volume. }
	
    \item{\code{surfaceArea}:}{Object of class \code{"numeric"}: Total
      log surface area in the appropriate units.}
	
    \item{\code{coverageArea}:}{Object of class \code{"numeric"}: Total
      log coverage area in the appropriate units.}
	
    \item{\code{biomass}:}{Object of class \code{"numeric"}: Total log
      biomass. This will be \code{NA} if no conversion was specified at
      object creation.}
	
    \item{\code{carbon}:}{Object of class \code{"numeric"}: Total log
      carbon content. This will be \code{NA} if no conversion was
      specified at object creation.}
	
    \item{\code{conversions}:}{Object of class \code{"numeric"}: A
      vector with names \code{c('volumeToWeight','weightToCarbon')}
      specifying the conversion factors for woody biomass and carbon
      content. }
	  
    \item{\code{taper}:}{Object of class \code{"data.frame"}: The log's
	  taper, either as specified from measurements, or as generated via
	  \code{\link{downLog}}. }
	
    \item{\code{profile}:}{Object of class \code{"data.frame"}: The log
	  profile as generated from the taper. The log is assumed to be
	  oriented with the small-end North, and the base at the origin. }
	
    \item{\code{rotLog}:}{Object of class \code{"matrix"}: A rotated and
	  translated version of \code{profile} using \code{logAngle} and
	  \code{location} fields. }
	
    \item{\code{spLog}:}{Object of class \code{"SpatialPolygons"}: A
	  \code{\linkS4class{SpatialPolygons}} representation of \code{rotLog} via the
	  \code{\link{sp}} package. }
	
    \item{\code{slNeedleAxis}:}{Object of class \code{"SpatialLines"}: A
	  fully transformed (rotated and translated) representation of the
	  needle for the log as a \code{\linkS4class{SpatialLines}} object
	  from package \code{\link{sp}}. }
	
  }
}
\section{Extends}{
Class \code{"\linkS4class{Stem}"}, directly.
}
\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "downLog")}: Returns the bounding
      box for the object. }
	
    \item{chainSawIZ}{\code{signature(downLog = "downLog", plotRadius =
		"numeric")}: Chainsaw method inclusion zone constructor. }
	
    \item{plot}{\code{signature(x = "downLog", y = "missing")}:
      Graphically display a "downLog" object. }
	
    \item{sausageIZ}{\code{signature(downLog = "downLog", plotRadius =
		"numeric")}: Sausage sampling inclusion zone constructor. }
	
    \item{standUpIZ}{\code{signature(downLog = "downLog", plotRadius =
		"numeric")}: Stand-up method inclusion zone constructor. }
	
    \item{summary}{\code{signature(object = "downLog")}: Summary of the
      object. }
	
	%note: show() defaults to summary for downLogs, so no explicit
	%method here
	 }
   }
   
\references{
Gove, J. H., Williams, M. S., Stahl, G. and Ducey, M. J. 2005. Critical
point relascope sampling for unbiased volume estimation of downed coarse
woody debris. \emph{Forestry} \bold{78}:417--431. \cr
%
Gove, J. H. and Van Deusen, P. C. 2011. On fixed-area plot sampling for
downed coarse woody debris. \emph{Forestry} \emph{Forestry} \bold{84}:109--117.\cr
%
Van Deusen, P.C. 1990. Critical height versus importance sampling for
log volume: does critical height prevail? \emph{Forest Science}\cr
\bold{36}(4):930--938. 
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
"\code{\linkS4class{Stem}}", "\code{\linkS4class{standingTree}}",
 "\code{\linkS4class{StemContainer}}"
}

\examples{
showClass("downLog")
}
\keyword{classes}
