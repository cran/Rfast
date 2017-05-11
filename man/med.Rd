\name{Median of a vector}
\alias{med}
\title{
Median of a vector
}

\description{
Median of a vector.
}

\usage{
med(x)
}

\arguments{
\item{x}{
A numerical vector.
}
}

\details{
The function is written in C++ and this is why it is very fast.  
}

\value{
The median of the vector of a numbers.
}

%\references{
%Draper, N.R. and Smith H. (1988). Applied regression analysis. New York, Wiley, 3rd edition.
%}

\author{
Manos Papadakis <papadakm95@gmail.com>

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{nth}, \link{colMedians}
}
}

\examples{
x <- rnorm(1000)
system.time( for (i in 1:100) med(x) )
system.time( for (i in 1:100) median(x) )
}

\keyword{ nth elements }
\keyword{ sorting } 