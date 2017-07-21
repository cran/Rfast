\name{Cholesky decomposition of a square matrix}
\alias{cholesky}
\title{
Cholesky decomposition of a square matrix
}

\description{
Cholesky decomposition of a square matrix.
}

\usage{
cholesky(x,parallel = FALSE)
}

\arguments{
\item{x}{
A square positive definite matrix.
}
\item{parallel}{
A boolean value for parallel version.
}
}

\details{
The Cholesky decomposition of a square positive definite matrix is computed.
}

\value{
An upper triangular matrix.
}

%\references{
%
%}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{is.symmetric}
}
}

\examples{
x = matrix(rnorm(1000 * 50), ncol = 50)
s = cov(x)
system.time(a1 <- cholesky(s))
system.time(a2 <- chol(s))
all.equal(a1[upper.tri(a1)], a2[upper.tri(a2)])
}

\keyword{ Cholesky decomposition }
