\name{writeCode}
\alias{writeCode}
\title{Generate JavaScript code to draw plots}
\description{
  This function is not necessarily called directly by the R user
  but is used in \code{\link{jsCanvas}} and \code{\link{htmlCanvas}}
  as the \code{wrapup} function when the graphics device is closed.
  This processes the different "pages" of plots and generates
  JavaScript functions from these "pages".

  This is documented so that users can understand the arguments that can
  be passed to it via the \dots in \code{jsCanvas} and \code{htmlCanvas}
  and also so programmers can use it in other devices or \code{wrapup}
  functions. For example, see \code{htmlWrapup}. 
}
\usage{
writeCode(pages, file = character(), canvasId = "canvas", funName = "rdraw", multiCanvas = FALSE, jsURLs = JSCode(), inlineJS = TRUE)
}
\arguments{
  \item{pages}{a list of }
  \item{file}{the name of the file or a connection to which the code is
    written. If this is missing or has length 0, the code is written to
    a local text connection and the resulting code returned as a
    character vector.}
  \item{canvasId}{a character vector giving the identifiers/names of the
  canvases. This can be a single value which is used as a prefix in the
  case of multiple plots.}
  \item{funName}{the name of the JavaScript functions to create. A
    single value is used as a prefix in the case of multiple plots.}
  \item{multiCanvas}{a logical value indicating whether the plots are to
    be drawn on separate canvases or all plots are to be drawn
    on the same canvas, at different times.
 }
 \item{jsURLs}{character vector giving the names of the files or URLs
    of supporting JavaScript code that needs to be loaded by the Web
    browser for the plots  to be rendered.
}
  \item{inlineJS}{a logical valued indicating whether to inline the
    contents of the \code{jsURLs} files.}
}
\value{
  Either the names of the functions that were created or
  if \code{file} has length 0, the code itself.
}
%\references{}
\author{
Duncan Temple Lang
}


\seealso{
  \code{\link{jsCanvas}}
 \code{\link{htmlCanvas}}  
}
\examples{

}
\keyword{graphics}
\keyword{device}
