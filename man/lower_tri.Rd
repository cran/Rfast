\name{Lower triangular of a (square) matrix}
\alias{lower_tri}
\alias{upper_tri}
\title{
Lower triangular of a (square) matrix or 
upper triangular of a (square) matrix 
}

\description{
Lower triangular of a (square) matrix.
Upper triangular of a (square) matrix
}

\usage{
lower_tri(x,value)
upper_tri(x,value)
}

\arguments{
\item{x}{
A matrix. 
}
\item{value}{
A logical value for choosing to return the values of the 
lower triangular and note a logical matrix. 
}
}

\details{
The algorithm is written in C++ to speed up things.
}

\value{
A logical matrix TRUE in the position of the triangular 
matrix or a vector with the lower triangular elements of a matrix .
}

%\references{
%}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadak95@gmail.com> 
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{sort_mat}
}
}

\examples{
x <- matrix(rnorm(500 * 500), ncol = 500)
system.time( for (i in 1:100) lower_tri(x) )
system.time( for (i in 1:100) lower_tri(x,TRUE) )
system.time( for (i in 1:100) upper_tri(x) )
system.time( for (i in 1:100) upper_tri(x,TRUE) )
}

\keyword{ matrix }
\keyword{ lower triangular }