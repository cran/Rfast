\name{Column and rows-wise mean absolute deviations}
\alias{colMads}
\title{
Column and row-wise mean absolute deviations
}

\description{
Column and row-wise mean absolute deviations.
}

\usage{
colMads(x)
}

\arguments{
\item{x}{
A matrix with the data.
}
}

\details{
The functions is written in C++ in order to be as fast as possible.
}

\value{
A vector with the column-wise mean absolute deviations.
}

%\references{
%Tsagris M.T., Preston S. and Wood A.T.A. (2011). A data-based power transformation for compositional data. In Proceedings of the 4th Compositional Data Analysis Workshop, Girona, Spain.
%}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{colMedians}, \link{rowMedians}, \link{colVars}, \link{colmeans}, \link{colMeans} (buit-in R function)
}
}

\examples{
x <- matrix( rnorm(100 * 1000), ncol = 1000 )
system.time( a <- colMads(x) )

}

\keyword{ Column-wise median absolute deviations }