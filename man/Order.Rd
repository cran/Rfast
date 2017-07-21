\name{ Order }
\alias{Order}
\title{
Order
}

\description{
Order function like R's order.
}

\usage{
Order(x,stable=FALSE)
}

\arguments{
\item{x}{
A Numeric vector with data. 
}
\item{stable}{
A boolean value for using a stable sorting algorithm. 
}
}

\details{
This function implements the R's "Order" function. If you want the same results as R's, then set "stable=TRUE" because "stable=FALSE" uses a sorting algorithm that it is not stable like R's sort. But it is faster to use the default. This verion is faster for large data, more than 1000.
}

\value{
Sort the vector and returns the indices of each element that it has before the sorting.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{Match}
}
}

\examples{
y <- rnorm(100)
b <- Order(y)
a <- order(y)
all.equal(a,b) ## false because it is not stable
a <- Order(y,TRUE)
all.equal(a,b) ## true because it is stable
}

\keyword{ Order Function }