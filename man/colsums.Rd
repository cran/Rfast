\name{ The sums of the collumns }
\alias{colsums}
\alias{colsums}
\title{
 The sums of the collumns.
}

\description{
The sums of the collumns.
}

\usage{
colsums(x)
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
Returns a row vector with collumn sums.
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
\code{\link{colsums}
}
}

\examples{
x <- matrix(rpois(1000 * 1000,10),ncol = 1000)
x1 <- colsums(x)
x2 <- colSums(x)
x1 == x2
}

\keyword{ Collumn Sums }

