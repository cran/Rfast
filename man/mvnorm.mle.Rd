\name{MLE of the multivariate normal distribution}
\alias{mvnorm.mle}
\title{
MLE of the multivariate normal distribution
}

\description{
MLE of the multivariate normal distribution.
}

\usage{
mvnorm.mle(x)
}

\arguments{
\item{x}{
A matrix with numerical data.
}
}

\details{
The mean vector, covariance matrix and the value of the log-likelihood is calculated.
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
Johnson Norman L., Kotz Samuel and Balakrishnan (1997). Discrete Multivariate Distributions. Wiley
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{multinom.mle}, \link{dmvnorm}, \link{gaussian.nb}
}
}

\examples{
x <- matrnorm(100, 4)
mvnorm.mle(x)
x <- NULL
}

\keyword{ Multivariate normal distribution }
