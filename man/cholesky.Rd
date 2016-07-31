\name{ Cholesky Matrix }
\alias{cholesky}
\title{
Cholesky Matrix .
}

\description{
Cholesky Matrix .
}

\usage{
cholesky(x)
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
A cholesky matrix.
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
\code{\link{chol}
}
}

\examples{
x <- cov(iris[ ,1:4])
F <- cholesky(x)
}

\keyword{ Cholesky Decomposition }

