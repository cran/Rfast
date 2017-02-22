\name{MLE of count data}
\alias{zip.mle}
\alias{ztp.mle}
\alias{negbin.mle}
\title{
MLE of count data
}

\description{
MLE of count data.
}

\usage{
zip.mle(x, tol = 1e-09)
ztp.mle(x, tol = 1e-09)
negbin.mle(x, type = 1, tol = 1e-09) 
}

\arguments{
\item{x}{
A vector with discrete valued data.
}
\item{type}{
This argument is for the negative binomial only and you can choose which way your prefer. Type 1 is for smal sample sizes, whereas type 2 is for larger ones as is faster. 
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
\item{mess}{
If there is no reason to use the negative binomial distribution a message will apepar, otherwise this is NULL.
}
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
Distributions (2nd ed.). Wiley
}

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
sum(dpois(x, mean(x), log = TRUE))
## small difference in the two log-likelihoods as expected.

x <- rpois(1000, 10)
x[x == 0 ] <- 1
ztp.mle(x)
sum(dpois(x, mean(x), log = TRUE))
## significant difference in the two log-likelihoods. 

x <- rnbinom(1000, 10, 0.6)
sum(dpois(x, mean(x), log = TRUE))
negbin.mle(x)
}

\keyword{ Zero inflated Poisson }
\keyword{ excessive zeros }
\keyword{ zero truncated Poisson }
\keyword{ negative binomial }
