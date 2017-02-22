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
colsums(x)

rowsums(x)
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
A vector with sums.
}


\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
\code{\link{colMedians}, \link{colmeans}, \link{colVars}
}
}

\examples{
x <- matrix(rpois(1000 * 1000,10),ncol = 1000)
x1 <- colsums(x)
x2 <- colSums(x)
all.equal(x1,x2)

x1 <- rowsums(x)
x2 <- rowSums(x)
all.equal(x1,x2)
}

\keyword{ Column sums }
\keyword{ Row sums }
