\name{The nth smallest value of a vector}
\alias{nth}
\title{
The nth smallest value of a vector
}

\description{
The nth smallest value of a vector.
}

\usage{
nth(x, k)
}

\arguments{
\item{x}{
A numerical vector.
}
\item{k}{
The kth smallest number to be returned. 
}
}

\details{
The function is written in C++ and this is why it is very fast. This is called (and used) by \code{\link{colMedians}}. 
}

\value{
The desired value.
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
\code{\link{med}, \link{colMedians}
}
}

\examples{
x <- rnorm(10000)
nth(x, 5000)
sort(x)[5000]
}

\keyword{ nth elements }
\keyword{ partial sorting } 