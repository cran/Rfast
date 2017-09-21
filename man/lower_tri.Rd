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
lower_tri(x, suma = FALSE)
upper_tri(x, suma = FALSE)
}

\arguments{
\item{x}{
A matrix with data \bold{or} a vector with 2 values which is the dimension of the logical matrix to be returned with the upper or lower triangular filled with \"TRUE\".
}
\item{suma}{
A logical value for returning the sum of the upper or lower triangular. By default is \"FALSE\".
Works only \bold{if} argument "x" is matrix.
}
}

%\details{
%
%}

\value{
Get a lower/upper triangular logical matrix with values \"TRUE\"/\"FALSE\"" or a vector with the values of a lower/upper triangular or the sum of the upper/lower triangular if suma is set \"TRUE\".
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

