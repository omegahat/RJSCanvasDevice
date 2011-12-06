\name{getCode}
\alias{getCode}
\alias{getCode,JavaScriptCanvasDevice-method}
\alias{JavaScriptCanvasDevice-class}
\alias{coerce,JavaScriptCanvasDevice,RJavaScriptCanvasMethods-method}
\title{Retrieve the generated code from a JavaScript canvas device}
\description{
  This function and methods provides a mechanism by which we can
  retrieve the generated JavaScript code from an existing
  JavaScript Canvas graphics device.
  The code is only generated when the device is closed, but
  we can retrieve the code from the device object after it is closed.

  NOTE:  the graphics device is being destroyed eventhough we have a
  reference to it in an R object.  We have to avoid this destruction
  in this case by putting our own finalizer.  For now,
  don't use getCode.
}
\usage{
getCode(from, ...)
}
\arguments{
  \item{from}{the \code{RJavaScriptCanvasDevice}}
  \item{\dots}{additional parameters for methods}
}
\value{
 A character vector
}
\author{
Duncan Temple Lang
}
\seealso{
 \code{\link{jsCanvas}}
}
\examples{
  dev = jsCanvas()
  r = dev$deviceSpecific$state@result
  plot(1:10)
  dev.off()
  r()
}
\keyword{graphics}
\keyword{device}
