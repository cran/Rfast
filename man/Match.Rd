\name{ Match }
\alias{Match}
\title{
Match
}

\description{
Return the positions of its first argument that matches in its second.
}

\usage{
Match(x,key)
}

\arguments{
\item{x}{
A numeric vector.
}
\item{key}{
The value/vector for searching in vector x.
}
}

\details{
This function implements the R's \"match\" function.
}

\value{
Returns the position/positions of the given key/keys in the x vector.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{match}
}
}

\examples{
y <- rnorm(100)
a <- Match(y,50)
b <-50%in%y
all.equal(as.vector(a),as.vector(b))
}

\keyword{ Match Function }