\name{ Sort and unique numbers }
\alias{sort_unique}
\title{
Sort and unique
}

\description{
Sort and unique numbers.
}

\usage{
sort_unique(x)
}

\arguments{
\item{x}{
A Numeric vector. 
}
}


\details{
This function implements the R's "unique" function using the C++'s.
}

\value{
Returns the discrete values but sorted.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{sort_mat}, \link{sort_index}, \link{sort_cor_vectors}
}
}

\examples{
y <- rnorm(100)
a <- sort_unique(y)
b <- sort.int(unique(y))
all.equal(as.vector(a),as.vector(b))
}

\keyword{ Sort Function }
\keyword{ Sort Unique }