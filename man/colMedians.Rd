\name{Column and row-wise medians}
\alias{colMedians}
\alias{rowMedians}
\title{
Column and row-wise medians
}

\description{
Column and row-wise medians of a matrix.
}

\usage{
colMedians(x)
rowMedians(x)
}

\arguments{
\item{x}{
A matrix with the data.
}
}

\details{
The functions is written in C++ in order to be as fast as possible.
}

\value{
A vector with the column medians.
}

%\references{
%Tsagris M.T., Preston S. and Wood A.T.A. (2011). A data-based power transformation for compositional data. In Proceedings of the 4th Compositional Data Analysis Workshop, Girona, Spain.
%}

\author{
R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{med}, \link{colVars}, \link{colMeans} (buit-in R function)
}
}

\examples{
x <- matrix( rnorm(100 * 100), ncol = 100 )
a <- apply(x, 2, median) 
b1 <- colMedians(x) 
all.equal(as.vector(a), b1)
}

\keyword{ Column-wise medians }
\keyword{ Row-wise medians }
