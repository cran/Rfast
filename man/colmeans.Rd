\name{ The means of the collumns }
\alias{colmeans}
\alias{colmeans}
\title{
 The means of the collumns.
}

\description{
The means of the collumns.
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
Returns a row vector with collumn means.
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
\code{\link{colmeans}
}
}

\examples{
x <- matrix(rpois(1000 * 1000,10),ncol = 1000)
x1 <- colmeans(x)
x2 <- colMeans(x)
x1 == x2
}

\keyword{ Collumn Means }

