\name{Column and row-wise sums of a matrix}
\alias{colsums}
\alias{rowsums}
\title{
Column and row-wise sums of a matrix
}

\description{
Column and row-wise sums of a matrix.
}

\usage{
colsums(x,indices = NULL, parallel = FALSE, na.rm = FALSE, cores = 0)
rowsums(x,indices = NULL, parallel = FALSE, na.rm = FALSE, cores = 0)
}

\arguments{
\item{x}{
A numerical matrix with data.
}
\item{indices}{
An integer vector with the indices to sum the columns/rows.
}
\item{parallel}{
Do you want to do it in parallel in C++? TRUE or FALSE. Doens't work with argument "indices".
}
\item{na.rm}{
A logical value indicating to remove NAs. The algorithm run in parallel so do not use with option parallel.
}
\item{cores}{
Number of cores to use for parallelism. Valid only when argument parallel is set to TRUE. 
Default value is 0 and it means the maximum supported cores.
}
}

\value{
A vector with sums.
}


\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{colMedians}, \link{colmeans}, \link{colVars}
}
}

\examples{
x <- matrix(rpois(500 * 100, 10),ncol = 100)
x1 <- colsums(x)
x2 <- colSums(x)
all.equal(x1,x2)

x1 <- rowsums(x)
x2 <- rowSums(x)
all.equal(x1,x2)

x<-x1<-x2<-NULL
}

\keyword{ Column sums }
\keyword{ Row sums }
