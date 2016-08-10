\name{ The sums of the columns and rows }
\alias{colsums}
\alias{rowsums}
\title{
 The sums of the columns and rows .
}

\description{
The sums of the columns and rows .
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
Returns a row vector with column sums or a column vector with the row sums.
}


\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
\code{\link{colsums}
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
\keyword{ row sums }
