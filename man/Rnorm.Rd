\name{Random generation for the normal distribution}
\alias{Rnorm}
\title{
Random generation for the normal distribution
}

\description{
Random generation for the normal distribution.
}

\usage{
Rnorm(n)
}

\arguments{
\item{n}{
The size of the numbers you want to returned.
}
}

\details{
The function is written in C++ and this is why it is very fast. 
}

\value{
A vector with the column indices which are factor variables. If there are no factor variables it will return an empty vector. 
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
\code{\link{Rpois},\link{Runif}
}
}

\examples{
Rnorm(1000)
}

\keyword{ Normal }
