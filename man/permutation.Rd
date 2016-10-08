\name{ Permutation }
\alias{permutation}
\alias{permutation_next}
\alias{permutation_prev}
\title{
Permutation
}

\description{
Permute the given vector.
}

\usage{
permutation(x, all)
permutation_next(x, all_next=TRUE)
permutation_prev(x, all_prev=TRUE)
}

\arguments{
\item{x}{
A numeric vector with data. 
}
\item{all}{
A logical value for returning all or one possible combinations. 
}
\item{all_next}{
A logical value for returning all the next or one possible combinations. 
}
\item{all_prev}{
A logical value for returning all the previous or one possible combinations. 
}
}

\details{
This function implements "Permutation" which means all the possible combinations.
}

\value{
Returns a matrix with all possible combinations of the given vector.
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
b <- permutation_next(y)
b <- permutation_prev(y)
}

\keyword{ Permutation Function }