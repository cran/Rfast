\name{ Permutation }
\alias{permutation}
\alias{permutation.next}
\alias{permutation.prev}
\alias{bincomb}
\title{
Permutation
}

\description{
Permute the given vector.
}

\usage{
permutation(x, all=TRUE)
permutation.next(x, all.next=TRUE)
permutation.prev(x, all.prev=TRUE)
bincomb(n)
}

\arguments{
\item{x}{
A numeric vector with data. 
}
\item{all}{
A logical value for returning all or one possible combinations. 
}
\item{all.next}{
A logical value for returning all the next or one possible combinations, if there are. 
}
\item{all.prev}{
A logical value for returning all the previous or one possible combinations, if there are. 
}
\item{n}{
An integer value for the length of the binary number.
}
}

\details{
This function implements "Permutation", which means all the possible combinations. In the permutation.next and permutation.prev
if there aren't possible combinations it returns the same vector. "Binary Combinations" for "bincomb", means all the possible combinations for the binary number with length "n".
}

\value{
Returns a matrix with all possible combinations of the given vector or a matrix row with one possible combinations.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{combn},\link{comb_n}
}
}

\examples{
y <- rnorm(5)
b <- permutation(y)
b <- permutation.next(y)
b <- permutation.prev(y)
g <- bincomb(4)
}

\keyword{ Permutation Function }