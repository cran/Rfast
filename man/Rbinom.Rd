\name{Random generation for the binomial distribution with parameters size and prob}
\alias{Rbinom}
\title{
Random generation for the binomial distribution with parameters size and prob
}

\description{
Random generation for the binomial distribution with parameters size and prob.
}

\usage{
Rbinom(n,nin,pp)
}

\arguments{
\item{n}{
The size of the vector to be returned.
}
\item{nin}{
The size.
}
\item{pp}{
Probubility.
}
}

\details{s
The function is written in C++ and this is why it is very fast. 
}

\value{
A row vector which is been initialized with binomial's numbers. 
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
Rbinom(1000,10,0.5)
}

\keyword{ Binomial}
