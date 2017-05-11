\name{MLE of the beta distribution}
\alias{beta.mle}
\title{
MLE of the beta distribution
}

\description{
MLE of the beta distribution.
}

\usage{
beta.mle(x, tol = 1e-09)
}

\arguments{
\item{x}{
A numerical vector with proportions, i.e. numbers in (0, 1) (zeros and ones are not allowed).
}
\item{tol}{
The tolerance level up to which the maximisation stops.
}
}

\details{
Maximum likelihood estimation of the parameters of the beta  distribution is performed via Newton-Raphson. The distributions and hence the functions 
does not accept zeros. 
}

\value{
A list including:
\item{iters}{
The number of iterations required by the Newton-Raphson.
}
\item{loglik}{
The value of the log-likelihood.
}
\item{param}{
The estimated parameters. 
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
\code{ \link{diri.nr2} 
}
}

\examples{
x <- rbeta(1000, 1, 4)
beta.mle(x)
}

\keyword{ Beta distribution }
\keyword{ Newton-Raphson }
\keyword{ proportions }
