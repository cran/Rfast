\name{Column and row-wise order}
\alias{colOrder}
\alias{rowOrder}
\title{
Column and row-wise order
}

\description{
Column and row-wise order.
}

\usage{
colOrder(x,stable=FALSE)
rowOrder(x,stable=FALSE)
}

\arguments{
\item{x}{
A matrix with numbers.
}
\item{stable}{
A boolean value for using a stable sorting algorithm. 
}
}

\details{
The function applies \"order\" in a column or row-wise fashion.
}

\value{
A matrix with integer numbers. The result is the same as apply(x, 2, order) or apply(x, 1, order).
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
}

\keyword{ Orderings }
\keyword{ matrix }
