\name{Column and row-wise Order - Sort Indices}
\alias{colOrder}
\alias{rowOrder}
\alias{Order}
\title{
Column and row-wise Order - Sort Indices
}

\description{
Column and row-wise Order - Sort Indices.
}

\usage{
colOrder(x,stable=FALSE,descending=FALSE)
rowOrder(x,stable=FALSE,descending=FALSE)
Order(x,stable=FALSE,descending=FALSE)
}

\arguments{
\item{x}{
A matrix with numbers or a numeric/character vector.
}
\item{stable}{
A boolean value for using a stable sorting algorithm. 
}
\item{descending}{
A boolean value (TRUE/FALSE) for sorting the vector in descending order. By default sorts the vector in ascending.
}
}

\details{
The function applies "order" in a column or row-wise fashion or Order a vector. If you want the same results as R's, then set "stable=TRUE" because "stable=FALSE" uses a sorting algorithm that it is not stable like R's sort. But it is faster to use the default. This verion is faster for large data, more than 300.
}

\value{
For "colOrder" and "Order" a matrix with integer numbers. The result is the same as apply(x, 2, order) or apply(x, 1, order).

For "Order" sort the vector and returns the indices of each element that it has before the sorting.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{colsums}, \link{coldiffs}, \link{colMedians}, \link{colprods}
}
}

\examples{
x <- matrix( runif(10 * 10), ncol = 10 )
colOrder(x)
apply(x, 2, order)
rowOrder(x)
t(apply(x, 1, order))

y <- sample(rnorm(100),100,TRUE)
b <- Order(y)
a <- order(y)
all.equal(a,b) ## false because it is not stable
a <- Order(y,stable=TRUE)
all.equal(a,b) ## true because it is stable
}

\keyword{ Orderings }
\keyword{ matrix }
