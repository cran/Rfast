\name{Generate keys for the given values using Paul Larsson's function}
\alias{generate_key_1}
\title{
Generate keys for the given values using Paul Larsson's function
}

\description{
Generate keys for the given values using Paul Larsson's function.
}

\usage{
generate_key_1(x)
}

\arguments{
\item{x}{
A character vector with values.
}
}

\details{
This function works like hash function but it only creates the keys and returns it to the user}

\value{
Return a vector with keys created by the given values.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{generate_key_2}
}
}

\examples{
x <- as.character(iris[, 5])
keys <- generate_key_1(x)
}

\keyword{ Hash Function }
\keyword{ Generator for keys }
