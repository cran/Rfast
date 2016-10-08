\name{Column and row-wise means of a matrix}
\alias{colmeans}
\alias{rowmeans}
\title{
Column and row-wise means of a matrix
}

\description{
Column and row-wise means of a matrix.
}

\usage{
colmeans(x)
rowmeans(x)
}

\arguments{
\item{x}{
A numerical matrix with data.
}
}

%\details{
%
%}

\value{
A vector with column means or a column vector with the row means.
}

%\references{
%Shabalin, A.A. (2012). Matrix eQTL: Ultra fast eQTL analysis %via large matrix operations. Bioinformatics 28(10): 1353-1358.
%}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
\code{\link{colsums}, \link{rowsums}, \link{colMins}, \link{colMedians}, \link{colMads}
}
}

\examples{
x <- matrix(rpois(1000 * 1000,10),ncol = 1000)
x1 <- colmeans(x)
x2 <- colMeans(x)
all.equal(x1,x2)

x1 <- rowmeans(x)
x2 <- rowMeans(x)
all.equal(x1,x2)
}

\keyword{ Column means }
\keyword{ row means }

