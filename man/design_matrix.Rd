\name{ Design Matrix }
\alias{design_matrix}
\title{
Design Matrix
}

\description{
Design Matrix.
}

\usage{
design_matrix(x, ones = TRUE)
}

\arguments{
\item{x}{
A character vector or a factor type vector. Do not supply a numerical vector. 
}
\item{ones}{
A boolean variable specifying whether to include the ones in the design matrix or not. The default value is TRUE. 
}
}

\details{
This function implements the R's "model.matrix" function and is used only when the x is a factor. 
}

\value{
Returns the same matrix with model.matrix.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{model.matrix}
}
}

\examples{
a <- design_matrix( iris[, 5] )
b <- model.matrix( ~ iris[,5] )  ## R's built-in funciton
all.equal(as.vector(a),as.vector(b)) ## true
}

\keyword{ Design Matrix }

