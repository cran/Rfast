\name{MLE of some circular distributions}
\alias{vm.mle}
\alias{wrapcauchy.mle}
\title{
MLE of some circular distributions
}

\description{
MLE of some circular distributions.
}

\usage{
wrapcauchy.mle(x, tol = 1e-09)
vm.mle(x, tol = 1e-09) 
}

\arguments{
\item{x}{
A numerical vector with the circular data. They must be expressed in radians. 
}
\item{tol}{
The tolerance level to stop the iterative process of finding the MLEs. 
}
}

\details{
The parameters of the von Mises and wrapped Cauchy distributions are estimated. For the Wrapped Cauchy, 
the iterative procedure described by Kent and Tyler (1988) is used. As for the von Mises distribution, 
we use a Newton-Raphson to estimate the concentration parameter. 
}

\value{
A list including:
\item{iter}{
The iterations required until convergence. This is returned in the wrapped Cauchy distribution only. 
}
\item{loglik}{
The value of the maximised log-likelihood.
}
\item{param}{
A vector consisting of the estimates of the two parameters, the mean direction for both distributions 
and the concentration parameter kappa and the rho for the von Mises and wrapped Cauchy respectively.
}
}

\references{
Mardia K. V. and Jupp P. E. (2000). Directional statistics. Chicester: John Wiley \& Sons. 

Sra S. (2012). A short note on parameter approximation for von Mises-Fisher distributions: 
and a fast implementation of Is(x). Computational Statistics, 27(1): 177-190. 

Kent J. and Tyler D. (1988). Maximum likelihood estimation for the wrapped Cauchy distribution.
Journal of Applied Statistics, 15(2): 247--254.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr>
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{\link{vmf.mle}, \link{rvonmises}, \link{rvmf}
}
}

\examples{
y <- rcauchy(100, 3, 1)
x <- y %% (2 * pi)
vm.mle(x)
wrapcauchy.mle(x)
}

\keyword{ von Mises distribution }
\keyword{ wrapped Cauchy distribution }
\keyword{ circular data }