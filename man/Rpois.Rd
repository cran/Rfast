\name{Random generation for the Poisson distribution with parameter lambda}
\alias{Rpois}
\title{
Random generation for the Poisson distribution with parameter lambda
}

\description{
Random generation for the Poisson distribution with parameter lambda.
}

\usage{
Rpois(n,mu)
}

\arguments{
\item{n}{
The size of the numbers you want for return.
}
\item{mu}{
Lambda parameter.
}
}

\details{
The function is written in C++ and this is why it is very fast. 
}

\value{
A row vector which is been initialized with poisson's numbers. 
}

%\references{
%Draper, N.R. and Smith H. (1988). Applied regression analysis. New York, Wiley, 3rd edition.
%}

\author{
Manos Papadakis <papadakm95@gmail.com>

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{Rnorm},\link{Runif}
}
}

\examples{
Rpois(1000,10)
}

\keyword{ Poisson }
