\name{Uniform distribution on the interval from min to max}
\alias{Runif}
\title{
Uniform distribution on the interval from min to max
}

\description{
Uniform distribution on the interval from min to max.
}

\usage{
Runif(n,mn,mx)
}

\arguments{
\item{n}{
The size of the numbers you want for return.
}
\item{mn}{
Min number.
}
\item{mx}{
Max number.
}
}

\details{
The function is written in C++ and this is why it is very fast. 
}

\value{
A row vector which is been initialized with uniform numbers. 
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
\code{\link{Rnorm},\link{Rpois}
}
}

\examples{
Runif(1000,1,10)
}

\keyword{ Uniform }
