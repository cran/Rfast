\name{Fitting a Dirichlet distribution via Newton-Rapshon}
\alias{diri.nr2}
\title{
Fitting a Dirichlet distribution via Newton-Rapshon
}

\description{
Fitting a Dirichlet distribution via Newton-Rapshon.
}

\usage{
diri.nr2(x, tol = 1e-07)
}

\arguments{
\item{x}{
A matrix containing the compositional data. Zeros are not allowed.
}
\item{tol}{
The tolerance level idicating no further increase in the log-likelihood.
}
}

\details{
Maximum likelihood estimation of the parameters of a Dirichlet distribution is performed via Newton-Raphson.
Initial values suggested by Minka (2003) are used. 
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

%\seealso{
%\code{\link{diri.est}, \link{diri.contour} \link{rdiri}
%}
%}

\examples{
x <- matrix( rgamma(100 * 4, c(5, 6, 7, 8), 1), ncol = 4)
x <- x / as.vector(rowsums(x))
system.time( for(i in 1:1000) diri.nr2(x) )
}

\keyword{ Dirichlet distribution }
\keyword{ maximum likelihood estimation }
\keyword{ Newton-Raphson }
