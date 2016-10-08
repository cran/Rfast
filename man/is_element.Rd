\name{Find element}
\alias{is_element}
\title{
Find element
}

\description{
Find element. 
}

\usage{
is_element(x, key)
}

\arguments{
\item{x}{
A vector or matrix with the data.
}
\item{key}{
A value to check if exists in the vector x.
}
}

\details{
The functions is written in C++ in order to be as fast as possible.
}

\value{
Search if the v exists in x linear time. Then returns TRUE/FALSE if the value is been found.
}

%\references{
%}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{ \link{binary_search} (buit-in R function)
}
}

\examples{
x <- rnorm(10000)
key <- x[500]
system.time( b <- is_element(x,key) )
}

\keyword{ Find element }
\keyword{ Divide and Qonquer }

