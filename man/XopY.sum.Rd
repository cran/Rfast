\name{Operations between two matrices}
\alias{XopY.sum}
\title{
Operations between two matrices
}

\description{
Operations between two matrices.
}

\usage{
XopY.sum(x, y = NULL, oper = "*")
}

\arguments{
\item{x}{
A numerical matrix.
}
\item{y}{
A second numerical matrix whose dimensions must match the ones of x.
}
\item{oper}{
The operation to be performed, either \"*\", \"/\", \"+\" or \"-\".
}
}

\details{
This function simply does operations between two matrices. 
}

\value{
Sum of \"x*x\" or \"x*y\".
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{Dist}, \link{dista}, \link{colmeans}
}
}

\examples{
x <- matrix( rnorm(5 * 5), ncol = 5 )
y <- matrix( rnorm(5 * 5), ncol = 5 )
XopY.sum(x, y, oper = "*")
}

\keyword{ Sum }
\keyword{ operations }