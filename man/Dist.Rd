\name{Distance matrix}
\alias{Dist}
\title{
Distance matrix
}

\description{
Distance matrix.
}

\usage{
Dist(x, method = "euclidean", square = FALSE)
}

\arguments{
\item{x}{
A matrix with data. The distances will be calculated between pairs of rows. 
}
\item{method}{
This is either "euclidean", "mahnatatan" or "maximum". the "euclidean" is the usual Euclidean distance,
the "manhattan" is the absolute distance between the two vectors. The "maximum" refers to the 
maximum distance between the elements of two vectors x and y (supremum norm).
}
\item{square}{
If you choose "euclidean" as the method, then you can have the optino to return the 
squared Euclidean distances by setting this argument to TRUE. 
}
}

\details{
The distance matrix is computer with an extra argument for the Euclidean distances.
}

\value{
A square matrix with the pairwise distances. 
}

\references{
Mardia K. V., Kent J. T. and Bibby J. M. (1979). Multivariate Analysis. Academic Press. 
}

\author{
Manos Papadakis

R implementation and documentation: 
Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{colMedians}
}
}

\examples{
x <- matrix(rnorm(200*100),ncol=100)
system.time(a1 <- Dist(x))
system.time(a2 <- dist(x))
}

\keyword{ Distance matrix }
\keyword{ Euclidean distance }
\keyword{ Manhattan distance }