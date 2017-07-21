\name{ Lower and Upper triangular of a matrix/vector }
\alias{lower_tri}
\alias{upper_tri}
\title{
 Lower and Upper triangular of a matrix/vector
}

\description{
Lower/upper triangular matrix/vector.
}

\usage{
lower_tri(x)
upper_tri(x)
}

\arguments{
\item{x}{
A matrix with data.
}
}

%\details{
%
%}

\value{
Get a lower/upper trinagular matrix or a vector with the values of a lower/upper triangular.
}

%\references{
%Shabalin, A.A. (2012). Matrix/vector eQTL: Ultra fast eQTL analysis %via large matrix/vector operations. Bioinformatics 28(10): 1353-1358.
%}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
\code{\link{rowMins}, \link{colFalse}, \link{nth}, \link{rowrange}, \link{rowMedians}, \link{rowVars}, \link{sort_mat}, \link{colTrue}
}
}

\examples{

x <- matrix(runif(10*10),10,10)

all.equal(lower_tri(c(10,10)),lower.tri(x))

all.equal(lower_tri(x),x[lower.tri(x)])

#all.equal(upper_tri(c(10,10)),upper.tri(x))

#all.equal(upper_tri(x),x[upper.tri(x)])

}

\keyword{ Lower and Upper triangular of a matrix/vector }

