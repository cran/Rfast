\name{Distance variance and covariance}
\alias{dvar}
\alias{dcov}
\alias{bcdcor}
\title{
Distance variance and covariance
}

\description{
Distance variance and covariances.
}

\usage{
dvar(x, bc = FALSE)
dcov(x, y, bc = FALSE)
bcdcor(x, y)
}

\arguments{
\item{x}{
A numerical matrix or a vector.
}
\item{y}{
A numerical matrix or a vector.
}
\item{bc}{
If you want the bias-corrected distance correlation set this equal to TRUE.
}
}

\details{
The distance variance of a matrix/vector or the distance covariance of two matrices is calculated.
For the distance variance of a vector we use the fast method of Huo and Szekely (2016).
}

\value{
The distance covariance or distance variance.
}

\references{
Szekely G.J., Rizzo M.L. and Bakirov N.K.(2007). Measuring and Testing Independence 
by Correlation of Distances. Annals of Statistics, 35(6):2769--2794.

Szekely G. J. and Rizzo M. L. (2023). The Energy of Data and Distance Correlation. 
Chapman and Hall/CRC.
}

\author{
Manos Papadakis

R implementation and documentation: Michail Tsagris <mtsagris@uoc.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{dcor}, \link{edist}
}
}

\examples{
x <- as.matrix(iris[1:50, 1:4])
y <- as.matrix(iris[51:100, 1:4])
res <- dcov(x, y)
res <- dvar(x[, 1])
}

\keyword{ Distance variance }
\keyword{ Distance covariance }
