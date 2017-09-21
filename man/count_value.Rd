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
A vector with the data (numeric or character).
}
\item{value}{
The value to check its frequency in the vector "x".
}
}

\details{
The functions is written in C++ in order to be as fast as possible. The "x" and "value" must have the same type. The type can be numeric or character.
}

\value{
The frequency of a value in a vector in linear time.
}

%\references{
%}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{med}, \link{binary_search}, \link{Order}, \link{nth} 
}
}

\examples{
x <- rnorm(100)
value <- x[50]
system.time( b <- count_value(x,value) )
x <- sample(letters,replace=TRUE)
value <- "r"
system.time( g <- count_value(x,value) )
}

\keyword{ Count the frequency of a value }