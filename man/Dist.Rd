\name{Distance matrix}
\alias{Dist}
\alias{vecdist}
\title{
Distance matrix
}

\description{
Distance matrix.
}

\usage{
Dist(x, method = "euclidean", square = FALSE, p = 0)
vecdist(x)
}

\arguments{
\item{x}{
A matrix with data. The distances will be calculated between pairs of rows. 
In the case of \bold{vecdist} this is a vector.
}
\item{method}{
This is either "euclidean", "mahnatatan", "canberra1", "canberra2", "minimum", "maximum", "minkowski",
"bhattacharyya", "hellinger", "total_variation" or "kullback_leibler" or "jensen_shannon". 
The last two options are basically the same. 
}
\item{square}{
If you choose "euclidean" or "hellinger" as the method, then you can have the optino to return the 
squared Euclidean distances by setting this argument to TRUE. 
}
\item{p}{
This is for the the Mikowski, the power of the metric. 
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
\code{\link{dista}, \link{colMedians}
}
}

\examples{
x <- matrix(rnorm(50 * 10), ncol = 10)
a1 <- Dist(x)
a2 <- as.matrix( dist(x) )
}

\keyword{ Distance matrix }
\keyword{ Euclidean distance }
\keyword{ Manhattan distance }
\keyword{ Canberra distance }
\keyword{ Hellinger distance }