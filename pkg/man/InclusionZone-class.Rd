\name{InclusionZone-class}
\Rdversion{1.1}
\docType{class}
\alias{InclusionZone-class}
\alias{bbox,InclusionZone-method}
%\alias{plot,InclusionZone,missing-method}
\alias{show,InclusionZone-method}
\alias{summary,InclusionZone-method}

\title{Class "InclusionZone"}

\description{
  This is a virtual class that defines the root structure for inclusion
  zones used in areal sampling methods. In general, subclasses will use
  objects generated from both "\code{\linkS4class{ArealSampling}}"
  and "\code{\linkS4class{Stem}}" classes to form the functional
  object. Therefore, the object will be tied to the actual "Stem" object
  and sampling method in use. Hence, individual \dQuote{InclusionZone}
  objects will be made for each stem (i.e., downed log) in a collection
  or population when the final objective is a sampling surface simulation.
}

\section{Objects from the Class}{A virtual Class: No objects may be
  created from it.}

\section{Slots}{
  \describe{
    \item{\code{description}:}{Object of class \code{"character"}: Some
      descriptive text about this class. }
	
    \item{\code{units}:}{Object of class \code{"character"}: A character
	  string specifying the units of measure. Legal values are \dQuote{English}
	  and \dQuote{metric.} }

    \item{\code{bbox}:}{Object of class \code{"matrix"}: The bounding
      box enclosing the overall object and its inclusion zone. } 
	
    \item{\code{spUnits}:}{Object of class \code{"CRS"}: A valid string
	  of class "\code{\linkS4class{CRS}}" denoting the spatial units
	  coordinate system as in package \pkg{sp}. }
	
    \item{\code{puaBlowup}:}{Object of class \code{"numeric"}: The per
	  unit area blowup or expansion factor, based on an acre or hectare
	  depending upon the units of measure. }
	
    \item{\code{puaEstimates}:}{Object of class \code{"list"}: A list of
	  estimates for per unit area quantities associated with the \dQuote{Stem}
	  component of the object.  }
	
    \item{\code{userExtra}:}{Object of class \code{"ANY"}: This can be
      anything else the user wants to include with the object. Often it
      would be in the form of a list object. (This slot may be removed
      in the future.) }

  }
}


\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "InclusionZone")}: Return the
	  bounding box }
	
    \item{plot}{\code{signature(x = "InclusionZone", y = "missing")}:
      Plot the object }
	
    \item{show}{\code{signature(object = "InclusionZone")}: Show the
      object }
	
    \item{summary}{\code{signature(object = "InclusionZone")}: Object summary }
	 }
   }
   
%\references{
%%  ~~put references to the literature/web site here~~
%}
\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  For subclasses, see: \code{\linkS4class{downLogIZ}},
  \code{\linkS4class{standUpIZ}}, \code{\linkS4class{chainSawIZ}},
  \code{\linkS4class{sausageIZ}}, \code{\linkS4class{pointRelascopeIZ}};
  and the \code{\linkS4class{downLogIZs}} container class.  }

\examples{
showClass("InclusionZone")
}
\keyword{classes}
