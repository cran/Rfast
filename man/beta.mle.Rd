\name{Fitting a beta distribution via Newton-Rapshon}
\alias{beta.mle}
\title{
Fitting a beta distribution via Newton-Rapshon
}

\description{
Fitting a Dirichlet distribution via Newton-Rapshon.
}

\usage{
beta.mle(x)
}

\arguments{
\item{x}{
A numerical vector with proportions, i.e. numbers in (0, 1) (zeros and ones are not allowed).
}
}

\details{
Maximum likelihood estimation of the parameters of a beta distribution is performed via Newton-Raphson. The x values and 1 - x become 
a two-column matrix and the function \code{\link{diri.nr2}} is implemented. 
}

\value{
A list including:
\item{loglik}{
The value of the log-likelihood.
}
\item{param}{
The estimated parameters.
}
\item{runtime}{
The run time of the procedure.
}
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{diri.nr2}
}
}

\examples{
x <- rbeta(1000, 1,4)
system.time( for(i in 1:1000) beta.mle(x) )
}

\keyword{ Beta distribution }
\keyword{ maximum likelihood estimation }
\keyword{ Newton-Raphson }
