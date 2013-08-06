\name{bufferedTract-methods}
\docType{methods}
\alias{bufferedTract-methods}
\alias{bufferedTract,numeric,Tract-method}

\title{  Methods for "bufferedTract" Object Construction in package
  "sampSurf" }

\description{
There is only one existing method available for the
  \code{\link{bufferedTract}} generic function, which will generate valid
  objects of class \code{"\linkS4class{bufferedTract}"}.
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(bufferWidth = "numeric", tract =
	  "Tract")}}}{%\cr
  
      This method takes the form shown below\ldots}

  \item{}{\bold{usage\ldots}
    \preformatted{bufferedTract(bufferWidth,
              tract,
              runQuiet = FALSE,
              ... )}
	 
	\itemize{
	  \item{\code{bufferWidth}:}{ The width of the buffer in the
	  correct units. For "English" units, this would be feet, for
	  "metric," meters. The buffer must be smaller than the tract extents.}
	 
	  \item{\code{tract}:}{ A valid tract object to which we
		will add the internal buffer.}
	} %itemize
  } %item

}}


\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
