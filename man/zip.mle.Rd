\name{MLE of the zero-inflated Poisson distribution}
\alias{zip.mle}
\title{
MLE of the zero-inflated Poisson distribution
}

\description{
MLE of the zero-inflated Poisson distribution.
}

\usage{
zip.mle(x, tol = 1e-09) 
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
Instead of maximising the log-likelihood via a numerical optimiser 
we used a Newton-Raphson algorithm which is faster. 
See wikipedia for the equation to be solved. https://en.wikipedia.org/wiki/Zero-inflated_model  
In order to avoid negative values we have used link functions, log for the \eqn{lambda} and logit
for the \eqn{\pi} as suggested by Lambert (1992).
}

\value{
A vector with three elements, the \eqn{\lambda}, \eqn{\pi} 
(the proportion of excessive zeros) and the maximised log-likelihood. 
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
sum( dpois(x, mean(x), log = TRUE) )  ## poisson log-likelihood
## small difference in the two log-likelihoods as expected.

x <- rpois(1000, 10)
x[ sample(1:1000, 100) ] <- 0
zip.mle(x)
sum( dpois(x, mean(x), log = TRUE) )  ## poisson log-likelihood
## significant difference in the two log-likelihoods. 
}

\keyword{ Zero inflated poisson }
\keyword{ excessive zeros }