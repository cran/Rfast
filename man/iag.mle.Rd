\name{MLE of the spherical projected normal distribution}
\alias{iag.mle}
\title{
MLE of the spherical projected normal distribution
}

\description{
MLE of the spherical projected normal distribution.
}

\usage{
iag.mle(y, tol = 1e-07)
}

\arguments{
\item{y}{
A matrix with the data expressed in Euclidean coordinates, i.e. unit vectors.
}
\item{tol}{
The tolerance to accept that the E-M algorithm used to estimate the concentration parameter has converged.
}
}

\details{
MLE of the projected normal distribution, on the sphere, is implemented.
}

\value{
A list including:
\item{iters}{
The number of iteration required by the Newton-Raphson. 
}
\item{mesi}{
A matrix with two rows. The first row is the mean direction and the second is the mean vector. 
The first comes from the second by normalising to have unit length.
}
\item{param}{
A vector with the elements, the norm of mean vector, the log-likelihood and the log-likelihood of the spherical uniform distribution. 
The third value helps in case you want to do a log-likleihood ratio test for uniformity.
}
\item{kappa}{
The concentration parameter.
}
}

\references{
Mardia, K. V. and Jupp, P. E. (2000). Directional statistics. Chicester: John Wiley & Sons.

Paine P.J., Preston S.P., Tsagris M and Wood A.T.A. (2017). An Elliptically Symmetric Angular Gaussian Distribution. Statistics and Computing (To appear). 
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr>
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{vmf.mle}, \link{acg.mle}, \link{vm.mle}
}
}

\examples{
x <- as.matrix(iris[,1:3])
x <- x / sqrt( rowsums(x^2) )
iag.mle(x)
}

\keyword{ Maximum likelihood estimation }
\keyword{ spherical data }
\keyword{ projected normal distribution }
