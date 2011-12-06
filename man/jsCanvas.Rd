\name{jsCanvas}
\alias{jsCanvas}
\title{Open JavaScript Canvas Graphics Device}
\description{
  This function opens a new R graphics device and
  becomes the active device to which graphics
  commands are sent. This device maps the graphical
  commands to JavaScript code. When the device is closed,
  the JavaScript is either written to a file
  or made available as a character vector.
  This JavaScript code can then be added to an HTML document
  and rendered on a JavaScript canvas.
}
\usage{
jsCanvas(file = character(), dim = c(1000, 800), col = "black", fill = "transparent",
          ps = 10, wrapup = writeCode, 
          canvasId = "canvas", multiCanvas = FALSE, ...,
          runPlotCommand = !missing(file) && !is.character(substitute(file)))
}
\arguments{
  \item{file}{the name of a file or a connection object to which the
    generated JavaScript code will be written when the device is closed.
    This can be an empty vector (the default)  in which case the code is
    not written to a file. Instead, the code can be accessed from the
    device.
    This argument can also be an R call or expression and
    it will be evaluated and the resulting JavaScript code
    returned directly. This allows one to open, plot and close
    the device in a single action and retrieve the JavaScript
    code in one operation.
   }
  \item{dim}{the dimensions to use for the HTML canvas device. These are pixels.}
  \item{col}{the default drawing color}
  \item{fill}{the default fill color}
  \item{ps}{the initial point size}
  \item{wrapup}{a function that is invoked when the device is closed.
    This is called with a list of containing code for each of the
    separate "pages" created in R, i.e. each time we call NewFrame.
    The content of each element of the page list is a character vector
    containing the JavaScript code for that page.
    The second argument is the file or connection to which to write the
    generated JavaScript code. Additional parameters passed to
    \code{jsCanvas} are also passed directly in this function call.
    The default is to call \code{writeCode} in the package.
    This turns the code into functions. One might want to use the code
    directly, e.g. to evaluate the commands rather than defining a function.
  }
  \item{canvasId}{the identifier(s) (i.e. name(s)) of the canvases on
    which to draw.}
  \item{multiCanvas}{a logical value that controls whether the functions
    arrange to draw on the separate canvases for each plot (\code{TRUE})
    or on the same canvas (\code{FALSE})}  
  \item{\dots}{additional arguments that are passed on to the function
    specified by \code{wrapup}}
  \item{runPlotCommand}{this is a logical value that controls whether to
    interpret the \code{file} argument as R commands to run to create
    the plot or as the name of a file.
   }
}
%\details{}
\value{
 If \code{runPlotCommand} is \code{TRUE} (e.g. if one passes a
 non-literal value for \code{file}), a charcter vector
 containing the generated  JavaScript code is returned.
 
 Otherwise, an object of class \code{JavaScriptCanvasDevice}
 is returned. This contains a reference to the C-level
 data structure representing the device and one can access
 the fields in the device and the R functions that implement
 the device.
  
}
\references{
 R Internals Manual for details on graphics devices.
}
\author{
Duncan Temple lang
}


\seealso{
  \code{\link{htmlCanvas}}
  
  The RGraphics Device package.
}
\examples{

 js.code = jsCanvas({ plot(1:10); plot(density(rnorm(100)))})

 dev = jsCanvas()
  plot(1:10)
  plot(density(rnorm(100)))
 f = getCode(dev)  # currently need to return the function before device is closed
 dev.off()
 f()


 jsCanvas("myJSCode.js")
  plot(1:10)
  plot(density(rnorm(100)))  
 dev.off()


  # This version writes to a file, but also controls the name of the
  # javascript functions - myPlot1 and myPlot2 - and arranges that
  # each of them is to draw on the canvas named plotCanvas.
  # We can then call these JavaScript functions at different times and
  # they will draw on that canvas.
  # We also inline the supporting JavaScript code.
 jsCanvas("myJSCode.js", funName = "myPlot", canvasId = 'plotCanvas',
              multiCanvas = FALSE, inlineJS = TRUE)
  plot(1:10)
  plot(density(rnorm(100)))  
 dev.off()
}
\keyword{graphics}
\keyword{device}
\keyword{dynamic}


