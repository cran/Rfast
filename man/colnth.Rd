\name{Column and row-wise nth}
\alias{colnth}
\alias{rownth}
\title{
Column and row-wise nth
}

\description{
Column and row-wise nth of a matrix.
}

\usage{
colnth(x,elems)
rownth(x,elems)
}

\arguments{
\item{x}{
A matrix with the data.
}
\item{elems}{
Am integer vector with the kth smallest number to be returned for each column/row.
}
}

\details{
The functions is written in C++ in order to be as fast as possible.
}

\value{
A vector with the column/row nth.
}

%\references{
%}

\author{
R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{med}, \link{colMedians}, \link{colMeans} (buit-in R function)
}
}

\examples{
x <- matrix( rnorm(100 * 100), ncol = 100 )
elems <- rpois(100,10)
system.time( colnth(x, elems) )
elems <- rpois(100, 10)
system.time( rownth(x,elems) )
}

\keyword{ Column-wise nth }
\keyword{ Row-wise nth }