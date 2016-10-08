\name{ Design Matrix }
\alias{design_matrix}
\title{
Design Matrix
}

\description{
Design Matrix.
}

\usage{
design_matrix(x)
}

\arguments{
\item{x}{
A character vector 
}
}

\details{
This function implements the R's "model.matrix" function.
}

\value{
Returns the same matrix with model.matrix .
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
x1 <- iris[, 5]
x <- as.character(x1)
a <- design_matrix(x)
}

\keyword{ Desing Matrix }
\keyword{ Model Matrix}
