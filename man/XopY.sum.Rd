\name{Operations between two matrices}
\alias{XopY.sum}
\alias{eachrow}
\title{
Operations between two matrices
}

\description{
Operations between two matrices.
}

\usage{
XopY.sum(x, y = NULL, oper = "*")
eachrow(x,y,suma = FALSE,oper = "*")
}

\arguments{
\item{x}{
A numerical matrix.
}
\item{y}{
A second numerical matrix whose dimensions must match the ones of x.
}
\item{oper}{
The operation to be performed, either "*", "/", "+" or "-".
}
\item{suma}{
A logical value for sum the result.
}
}

\details{
XopY.sum: sum(X op Y) where op can be on of "+,-,*,/".

eachrow: X op Y or sum(X op Y) \bold{by row} where "x" is matrix, "y" is vector 
length as much an the columns of x and "op" is one of "+,-,*,/"
}

\value{
XopY.sum: sum(X op Y) where "op" can be on of "+,-,*,/".

eachrow: operation by row between a matrix and a vector."op" can be on of "+,-,*,/".
If "suma=TRUE" then returns the sum of this operation.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

\seealso{
\code{ \link{Dist}, \link{dista}, \link{colmeans},, \link{Diag.fill}, 
\link{colMads}, \link{rowMads}
}
}

\examples{
x <- matrix( rnorm(5 * 5), ncol = 5 )
y <- matrix( rnorm(5 * 5), ncol = 5 )
XopY.sum(x, y, oper = "*")
y <- x[,1]
eachrow(x,y)

x<-y<-NULL
}

\keyword{ Sum }
\keyword{ operations }