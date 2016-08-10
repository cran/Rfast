\name{Random generation for the Beta distribution with shape 1 and shape 2}
\alias{Rbeta}
\title{
Random generation for the Beta distribution with shape 1 and shape 2
}

\description{
Random generation for the Beta distribution with shape 1 and shape 2.
}

\usage{
Rbeta(n,a,b)
}

\arguments{
\item{n}{
The size of the numbers you want for return.
}
\item{a}{
Shape 1.
}
\item{b}{
Shape 2.
}
}

\details{
The function is written in C++ and this is why it is very fast. 
}

\value{
A row vector which is been initialized with beta's numbers. 
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
\code{\link{Rnorm},\link{Runif},\link{Rbinom}
}
}

\examples{
Rbeta(1000,1,10)
}

\keyword{ Betta numbers }
