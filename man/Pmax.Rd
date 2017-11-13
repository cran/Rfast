\name{Minima and maxima of two vectors}
\alias{Pmax}
\alias{Pmin}
\title{
Minima and maxima of two vectors
}

\description{
Minima and maxima of two vectors.
}

\usage{
Pmax(x, y)
Pmin(x, y)
}

\arguments{
\item{x}{
A numverical vector with numbers.
}
\item{y}{
A numverical vector with numbers. The lengths of these two vectorsa must match.
}
}

\details{
The parallel minima or maxima are returned. This are the same as the base functions pmax and pmin. 
}

\value{
A numerical vector with numbers, whose length is equal to the length of the initital vectors containing the
maximum or minimum between each pair. 
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

\seealso{
\code{ \link{sort_mat}, \link{Sort}, \link{colMins}
}
}

\examples{
x <- rnorm(10)
y <- rnorm(10)
Pmax(x, y)
pmax(x, y)
Pmin(x, y)
pmin(x, y)

x<-y<-NULL
}

\keyword{ Pairs of vectors }
\keyword{ minimum }
\keyword{ maximum }
