\name{Count the frequency of a value}
\alias{count_value}
\title{
Count the frequency of a value
}

\description{
Count the frequency of a value. 
}

\usage{
count_value(x, value)
}

\arguments{
\item{x}{
A vector with the data.
}
\item{value}{
A value to check how many times v exists in the vector x.
}
}

\details{
The functions is written in C++ in order to be as fast as possible.
}

\value{
Counts how many times a value exists in a vector in linear time.
}

%\references{
%}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{med}, \link{binary_search} 
}
}

\examples{
x <- rnorm(1000)
value <- x[50]
system.time( b <- count_value(x,value) )
}

\keyword{ Count the frequency of a value }

