\name{ Order }
\alias{Order}
\title{
Order
}

\description{
Order function like R's order.
}

\usage{
Order(x)
}

\arguments{
\item{x}{
A Numeric vector with data. 
}
}

\details{
This function implements the R's "Order" function.
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
}

\keyword{ Order Function }