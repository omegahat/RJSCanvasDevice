\name{htmlCanvas}
\alias{htmlCanvas}
\title{Create HTML document to draw one or more plots}
\description{
  This creates an HTML document that contains and displays
  the generated plots on JavaScript canvases.
}
\usage{
htmlCanvas(file, dim = c(1000, 800),
            template = system.file("template.html", package = "RJSCanvasDevice"),
             canvasId = "canvas", ...)
}
\arguments{
  \item{file}{the name of the file to which to write the HTML and
    JavaScript content.}
  \item{dim}{the dimensions of the JavaScript canvas on which the
    resulting plot will be displayed.}
  \item{template}{the name of the HTML file that serves as a template.}
  \item{canvasId}{the identifier(s) (i.e. name(s)) of the canvases on
    which to draw.}
  \item{\dots}{additional arguments passed on to \code{htmlWrapup} which
    writes the generated code to an HTML file.}
}
\details{
}
\value{
A reference to the C-level device.
}
\references{
 R Internals Manual for details on graphics devices.
}
\author{
Duncan Temple lang
}


\seealso{
  \code{\link{jsCanvas}}

  The RKMLDevice package
 
  The FlashMXML device and package.
}
\examples{

  # The basic version that creates one plot and draws it on the existing canvas
 dev = htmlCanvas("simple.html")
   plot(1:10)
 dev.off()


   # This changes the dimension of the existing canvas in the template
 dev = htmlCanvas("simpleDim.html", dim = c(500, 500))
   plot(1:10)
 dev.off()

  # This creates a new canvas, leaving the existing one there.
  # We may want to remove it.
 dev = htmlCanvas("simpleDim.html", dim = c(500, 500), canvasId = "plot")
   plot(1:10)
 dev.off()

   # Creates two plots, draws the first one and uses the existing
   # canvas.
 dev = htmlCanvas("foo.html") 
   plot(1:10)
   plot(density(rnorm(100)))
 dev.off()

   # Draw 2 plots, but use the same canvas. And change the dimension
   # We draw the first plot when the document is loaded.
   # The second can be drawn on the same canvas by calling the function
   #  rdraw2() at some point in time during the life of the document,
   #   e.g. on clicking a button.
 dev = htmlCanvas("foo.html", dim = c(500, 400) )
   plot(1:10)
   plot(density(rnorm(100)))
 dev.off()

    # add a button to toggle between the 2 plots
    # Define a variable to control which plot.
  library(XML)
  doc = htmlParse("foo.html")
  b = getNodeSet(doc, "//body")[[1]]
  addJSCode(doc, "var ctr = true;")
  newXMLNode("input", attrs = c(type = "button",
                                onclick = "if(ctr) rdraw2();  else rdraw1(); ctr = !ctr;",
                                value = "Draw next plot"),
               parent = b)
  saveXML(doc, "foo.html")
  

  # This specifies the dimensions for the two canvases
  # and indicates that we want to plot onto each.
 dev = htmlCanvas("foo.html", multiCanvas = TRUE,
                   dim = matrix(c(1000, 800,
                                  500, 400), 2, 2, byrow = TRUE))
   plot(1:10)
   plot(density(rnorm(100)))
 dev.off()
}
\keyword{graphics}
\keyword{device}
\keyword{dynamic}

