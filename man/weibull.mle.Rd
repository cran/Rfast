\name{MLE of the Weibull distribution}
\alias{weibull.mle}
\title{
MLE of the Weibull distribution}

\description{
MLE of the Weibull distribution.
}

\usage{
weibull.mle(x, tol = 1e-09)
}

\arguments{
\item{x}{
A vector with positive valued data (zeros are not allowed).
}
\item{tol}{
The tolerance level up to which the maximisation stops; set to 1e-09 by default.
}
}

\details{
Instead of maximising the log-likelihood via a numerical optimiser we have used a Newton-Raphson algorithm which is faster. See wikipedia for the equations to be solved. 
For the t distribution we need the degrees of freedom and estimate the location and scatter parameters. 
}

\value{
A list including 
\item{iters}{The number of iterations required for the Newton-Raphson to converge.
}
\item{loglik}{The value of the maximised log-likelihood.
}
\item{param}{The vector of the parameters.
}
}

\references{
Johnson, Norman L. Kemp, Adrianne W. Kotz, Samuel (2005). 
Univariate Discrete Distributions (third edition). Hoboken, NJ: Wiley-Interscience. 
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{zip.mle}, \link{negbin.mle}
}
}

\examples{
x <- rweibull(100, 1, 2)
for (i in 1:10) weibull.mle(x) 
# for (i in 1:10)  MASS::fitdistr(x, "weibull") )
}

\keyword{ Weibull distribution }
