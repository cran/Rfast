\name{ The means of the columns and the rows }
\alias{colmeans}
\alias{rowmeans}
\title{
 The means of the columns and the rows.
}

\description{
The means of the columns and the rows.
}

\usage{
colmeans(x)
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
Returns a row vector with column means or a column vector with the row means.
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
\code{\link{colsums},\link{rowsums},\link{colMins},\link{colMedians}
}
}

\examples{
x <- matrix(rpois(1000 * 1000,10),ncol = 1000)
x1 <- colmeans(x)
x2 <- colMeans(x)
x1 == x2
x1 <- rowmeans(x)
x2 <- rowMeans(x)
x1 == x2
}

\keyword{ column means }
\keyword{ row means }

