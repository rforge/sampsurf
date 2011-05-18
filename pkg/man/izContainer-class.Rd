\name{izContainer-class}
\Rdversion{1.1}
\docType{class}
\alias{izContainer-class}
\alias{hist,izContainer-method}
\alias{summary,izContainer-method}

\title{Class \code{"izContainer"}}

\description{ This is a virtual class whose purpose is to define a
  \emph{container} class for objects of class
  "\code{\linkS4class{InclusionZone}}." It provides slots and validity
  checks that would be common to all subclasses. Please note that this
  is not a completely functional container class in the traditional
  sense at present as it does not have replacement, deletion, or
  addition functions.  Please see \dQuote{\emph{The InclusionZone
  Class}} vignette for more information. }

\section{Objects from the Class}{
This is a virtual class, so no objects can be created of this
type. However, subclass objects can be created by their constructors
(see, e.g., "\code{\linkS4class{downLogIZs}}"). 
}

\section{Slots}{
  \describe{
	
    \item{\code{iZones}:}{Object of class \code{"list"}: This slot holds
	  a list of objects that all correspond to the same subclass of the
	  "\code{\linkS4class{InclusionZone}}" class. In other words, each item
	  must have been generated from the same sampling method; i.e., all
	  of class "\code{\linkS4class{sausageIZ}}," for example.  }
	
    \item{\code{units}:}{Object of class \code{"character"}: The same units
      as the "\code{\linkS4class{Stem}}" class objects in the
      collection; note that they must all be measured in the same
      units. }
	
    \item{\code{bbox}:}{Object of class \code{"matrix"}: The overall
      bounding box for the collection used in plotting. }
	
    \item{\code{description}:}{Object of class \code{"character"}: A
      description of the collection. }	

  }
}
\section{Methods}{
  \describe{
    \item{hist}{\code{signature(x = "izContainer")}: Display a histogram
	of inclusion zone areas in the collection. }
	
    \item{summary}{\code{signature(object = "izContainer")}: A summary
	 of the collection. }
	 }
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{downLogIZs}}  
}
\examples{
showClass("izContainer")
}
\keyword{classes}
