\name{MLE of (hyper-)spherical distributions}
\alias{vmf.mle}
\alias{acg.mle}
\title{
MLE of (hyper-)spherical distributions
}

\description{
MLE of (hyper-)spherical distributions.
}

\usage{
vmf.mle(x, tol = 1e-07)
acg.mle(x, tol = 1e-09)
}

\arguments{
\item{x}{
A matrix with directional data, i.e. unit vectors.
}
\item{tol}{
The tolerance value at which to terminate the iterations.  
}
}

\details{
For the von Mises-Fisher, the normalised mean is the mean direction. For the concentration parameter, a Newton-Raphson is implemented. 
For the angular central Gaussian distribution there is a constraint on the estimated covariance matrix; its trace is equal to the number of variables. 
An iterative algorithm takes place and convergence is guaranteed.
}

\value{
For the von Mises-Fisher a list including:
\item{loglik}{
The maximum log-likelihood value.
}
\item{mu}{
The mean direction.
}
\item{kappa}{
The concentration parameter.
}
For the angular central Gaussian a list including:
\item{iter}{
The number if iterations required by the algorithm to converge to the solution.
}
\item{cova}{
The estimated covariance matrix.
}
}

\references{
Mardia, K. V. and Jupp, P. E. (2000). Directional statistics. Chicester: John Wiley & Sons.

Sra, S. (2012). A short note on parameter approximation for von Mises-Fisher distributions: and a fast implementation of Is(x). 
Computational Statistics, 27(1): 177--190.

Tyler D. E. (1987). Statistical analysis for the angular central Gaussian distribution on the sphere.
Biometrika 74(3): 579-589.
}

\author{
Michail Tsagris
R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> 
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{racg}, \link{vm.mle}, \link{iag.mle}
}
}

\examples{
m <- c(0, 0, 0, 0)
s <- cov(iris[, 1:4])
x <- racg(100, s)
mod <- acg.mle(x)
mod
cov2cor(mod$cova)  ## estimated covariance matrix turned into a correlation matrix
cov2cor(s)  ## true covariance matrix turned into a correlation matrix
vmf.mle(x)
}

\keyword{ von Mises-Fisher distribution }
\keyword{ central angular Gaussian distribution }
\keyword{ maximum likelihood estimation }
\keyword{ directional data }
