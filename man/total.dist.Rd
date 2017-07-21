\name{Sum of all pairwise distances in a distance matrix}
\alias{total.dist}
\alias{total.dista}
\title{
Sum of all pairwise distances in a distance matrix
}

\description{
Sum of all pairwise distances in a distance matrix.
}

\usage{
total.dist(x)
total.dista(x, y)  
}

\arguments{
\item{x}{
A matrix with numbers.
}
\item{y}{
A second matrix with data. The number of comlumns of this matrix must be the same with the matrix x. The number of rows can be different. 
}
}

\details{
In order to do the total.dist one would have to calcualte the distance matrix and sum it. We do this internally in C++ without creating the matrix. 
For the total.dista it is the same thing.
}

\value{
A numerical value, the sum of the distances. 
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{Dist}, \link{dista}
}
}

\examples{
x <- matrix( rnorm(50 * 10), ncol = 10 )
total.dist(x) 
y <- matrix( rnorm(40 * 10), ncol = 10)
total.dista(x, y)
total.dista(y, x)
}

\keyword{ Distances}
\keyword{ total sum }
