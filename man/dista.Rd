\name{Distance between vectors and a matrix}
\alias{dista}
\title{Distance between vectors and a matrix
}

\description{Distance between vectors and a matrix.
}

\usage{
dista(xnew, x, type = "euclidean")
}

\arguments{
\item{xnew}{
A matrix with some data or a vector.
}
\item{x}{
A matrix with the data, where rows denotes observations (vectors) and the columns contain the variables.
}
\item{type}{
This can be either "euclidean" or "manhattan".
}
}

\details{
The target of this function is to calculate the distances between xnew and x without having to calculate the whole distance matrix of xnew and x. The latter does extra calculaitons, which can be avoided.
}

\value{
A matrix with the distances of each xnew from each vector of x. The number of rows of the xnew and and the number of columns of xnew are the dimensions of this matrix.
}

%\references{
%Tsagris M.T., Preston S. and Wood A.T.A. (2011). A data-based power transformation for compositional data. In Proceedings of the 4th Compositional Data Analysis Workshop, Girona, Spain.
%}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{mahala}
}
}

\examples{
xnew <- as.matrix( iris[1:10, 1:4] )
x <- as.matrix( iris[-c(1:10), 1:4] )
a <- dista(xnew, x)
b <- as.matrix( dist( rbind(xnew, x) ) )
b <- b[ 1:10, -c(1:10) ]
sum( abs(a - b) )

## see the time
x <- matrix( rnorm(1000 * 4), ncol = 4 )
system.time( dista(xnew, x) )
system.time( as.matrix( dist( rbind(xnew, x) ) ) )

}

\keyword{ Distances }

