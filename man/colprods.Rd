\name{Column and row-wise products}
\alias{colprods}
\alias{rowprods}
\title{
Column and row-wise products
}

\description{
Column and row-wise products.
}

\usage{
colprods(x)
rowprods(x)
}

\arguments{
\item{x}{
A matrix with numbers.
}
}

\details{
The product of the numbers in a matrix is returned either column-wise or row-wise.
}

\value{
A vector with the column or the row products.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

\seealso{
\code{ \link{colsums}, \link{coldiffs}, \link{colMedians}
}
}

\examples{
x <- matrix( runif(100 * 10), ncol = 10 )
colprods(x)
rowprods(x)

x<-NULL
}

\keyword{ Products }
\keyword{ matrix }
