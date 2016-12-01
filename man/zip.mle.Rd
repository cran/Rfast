\name{MLE of the zero inflated and zero truncated Poisson}
\alias{zip.mle}
\alias{ztp.mle}
\title{
MLE of the zero inflated and zero truncated Poisson
}

\description{
MLE of the zero inflated and zero truncated Poisson.
}

\usage{
zip.mle(x, tol = 1e-09)
ztp.mle(x, tol = 1e-09)
}

\arguments{
\item{x}{
A vector with discrete valued data.
}
\item{tol}{
The tolerance level up to which the maximisation stops set to 1e-09 by default.
}
}

\details{
Instead of maximising the log-likelihood via a numerical optimiser we used a Newton-Raphson algorithm which is faster. 

See wikipedia for the equation to be solved in the case of the zero inflated distribution. https://en.wikipedia.org/wiki/Zero-inflated_model. 
In order to avoid negative values we have used link functions, log for the \eqn{lambda} and logit for the \eqn{\pi} as suggested by Lambert (1992). 
As for the zero truncated Poisson see https://en.wikipedia.org/wiki/Zero-truncated_Poisson_distribution.
}

\value{
A list including:
\item{iters}{The number of iterations required for the Newton-Raphson to converge.
}
\item{loglik}{The value of the maximised log-likelihood.
}
\item{param}{The vector of the parameters for the zero inflated Poisson.
}
}

\references{
Lambert Diane (1992). Zero-Inflated Poisson Regression, with an Application to Defects in 
Manufacturing. Technometrics. 34 (1): 1-14

Johnson Norman L., Kotz Samuel and Kemp Adrienne W. (1992). Univariate Discrete 
Distributions (2nd ed.). Wiley}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{poisson_only}, \link{colrange}
}
}

\examples{
x <- rpois(100, 2)
zip.mle(x)
## small difference in the two log-likelihoods as expected.

x <- rpois(1000, 10)
x[x == 0 ] <- 1
ztp.mle(x)
## significant difference in the two log-likelihoods. 
}

\keyword{ Zero inflated Poisson }
\keyword{ excessive zeros }
\keyword{ zero truncated Poisson }

