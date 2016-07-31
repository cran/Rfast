\name{Mahalanobis distance}
\alias{mahala}
\title{Mahalanobis distance
}

\description{Mahalanobis distance.
}

\usage{
mahala(x, mu, sigma, ncores, ischol)
}

\arguments{
\item{x}{
A matrix with the data, where rows denotes observations (vectors) and the columns contain the variables.
}
\item{mu}{
The mean vector.
}
\item{sigma}{
The covariance or any square symmetric matrix.
}
\item{ncores}{
The numbers of cores to use
}
\item{ischol}{
A boolean variable set to true if the Cholesky decomposition of the covariance matrix is supplied in the argument "sigma".
}

}

%\details{
%For every value of \eqn{\alpha} the normal likelihood (see the refernece) is computed. At the end, the plot of the values is constructed.
%}

\value{
A vector with the Mahalanobis distances.
}

%\references{
%Tsagris M.T., Preston S. and Wood A.T.A. (2011). A data-based power transformation for compositional data. In Proceedings of the 4th Compositional Data Analysis Workshop, Girona, Spain.
%}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\examples{
x <- matrix( rnorm(10000 * 200), ncol = 200 )
m <- colMeans(x)
s <- cov(x)
system.time( mahala(x, m, s) )
system.time( mahalanobis(x, m, s) )
a1 <- mahalanobis(x, m, s)
a2 <- mahala(x, m, s)
all.equal(a1, a2)
}

\keyword{ Mahalanobis distance }

