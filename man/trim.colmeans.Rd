\name{Column and row-wise trimmed means of a matrix}
\alias{trim.colmeans}
\alias{trim.rowmeans}
\title{
Column and row-wise trimmed means of a matrix
}

\description{
Column and row-wise trimmed means of a matrix.
}

\usage{
trim.colmeans(x, trim = 0.05)
trim.rowmeans(x, trim = 0.05)
}

\arguments{
\item{x}{
A numerical matrix with data.
}
\item{trim}{
A number between 0 and 1. The percentage of trimming to apply.
}
}


\value{
A vector with the column or row trimmed means of the matrix.
}

%\references{
%Shabalin, A.A. (2012). Matrix eQTL: Ultra fast eQTL analysis %via large matrix operations. Bioinformatics 28(10): 1353-1358.
%}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{colsums}, \link{rowsums}, \link{colMins}, \link{colMedians}, \link{colMads}
}
}

\examples{
x <- Rfast::matrnorm(100, 10)
trim.colmeans(x, 0.05)
apply(x, 2, mean, trim = 0.05)
}

\keyword{ Column means }
\keyword{ row means }
\keyword{ harmonic means }