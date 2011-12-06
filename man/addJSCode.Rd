\name{addJSCode}
\alias{addJSCode}
\alias{addJSCode,HTMLInternalDocument,ANY-method}
\alias{addJSCode,XMLInternalNode,AsIs-method}
\alias{addJSCode,XMLInternalNode,character-method}
\title{Insert JavaScript code into a (HTML) document}
\description{
  This function and its methods add JavaScript code into an
  HTML document.
}
\usage{
addJSCode(to, code, src = character(), attrs = c(type = "application/x-javascript"), ...)
}
\arguments{
  \item{to}{the document into which the code is to be inserted}
  \item{code}{the code. Either a character vector or an \code{AsIs}
    character vector. If this is an \code{AsIs} object, the character
    vector is taken as the names of files or URLs and these are added as
   references to those files rather than inserting the contents of those
   objects. For regular character vectors, the contents are inserted.
  }
  \item{src}{the original name of the file from which the code came.
  This is used when \code{code} is not \code{AsIs} and it is addeds as
  the value of the attribute \code{originalURL}.}
  \item{attrs}{a named character vector giving the attributes for the
    \code{script} node being added.}
  \item{\dots}{additional parameters}
}
\value{
A new XML node that is added to the HTML document.
}
\author{
Duncan Temple Lang
}
\seealso{

}
\examples{

}

\keyword{programming}


