\name{Energy distance between two matrices}
\alias{edist}
\title{
Energy distance between two matrices
}

\description{
Energy distance between two matrices.
}

\usage{
edist(x, y)
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
This calculates the energy distance between two matrices. It will work even for tens of thousands of rows, it will just take some time. 
See the references for more information.
}

\value{
A numerical value, the energy distance. 
}

\references{
Szekely G. J. and Rizzo M. L. (2004) Testing for Equal Distributions in High Dimension, InterStat, November (5).

Szekely G. J. (2000) Technical Report 03-05, E-statistics: Energy of Statistical Samples, Department of Mathematics and 
Statistics, Bowling Green State University. 

Sejdinovic D., Sriperumbudur B., Gretton A. and Fukumizu, K. (2013). Equivalence of distance-based and
RKHS-based statistics in hypothesis testing. The Annals of Statistics, 41(5), 2263-2291.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{total.dist}, \link{total.dista}, \link{Dist}, \link{dista}
}
}

\examples{
x <- matrix( rnorm(50 * 10), ncol = 10 )
y <- matrix( rnorm(20 * 10), ncol = 10 ) 
edist(x, y)
edist(y, x)
}

\keyword{ Distances}
\keyword{ total sum }
