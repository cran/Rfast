\name{Internal function for the Dirichlet distibution}
\alias{diri_nr_type2}
\title{
Internal function for the Dirichlet distibution
}

\description{
Internal function for the Dirichlet distibution.
}

\usage{
diri_nr_type2(a1,a2,ma,tol)
}

\arguments{
\item{a1}{
A vector with initial values.
}
\item{a2}{
A second vector with initial values.
}
\item{ma}{
A vector with column-wise means of the logarithm of the compositional data.
}
\item{tol}{
The tolerance level idicating no further increase in the log-likelihood.
}
}

\details{
This function is to be called by the 
\code{\link{diri.nr2}} function. It is not to be called directly by the user unless you know what you are doing.
}

\value{
A column vector with the final parameters.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{diri.nr2}
}
}

\examples{
x <- matrix( rgamma(100 * 4, c(5, 6, 7, 8), 1), ncol = 4)
x <- x / rowSums(x)
system.time( for(i in 1:1000) diri.nr2(x) )
}

\keyword{ Dirichlet distribution }
\keyword{ maximum likelihood estimation }
\keyword{ Newton-Raphson }
