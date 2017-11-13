\name{ Lower and Upper triangular of a matrix }
\alias{lower_tri}
\alias{upper_tri}
\title{
 Lower and Upper triangular of a matrix
}

\description{
Lower/upper triangular matrix.
}

\usage{
lower_tri(x, suma = FALSE, diag = FALSE)
upper_tri(x, suma = FALSE, diag = FALSE)
}

\arguments{
\item{x}{
A matrix with data \bold{or} a vector with 2 values which is the dimension of the logical matrix to be returned with the upper or lower triangular filled with \"TRUE\".
}
\item{suma}{
A logical value for returning the sum of the upper or lower triangular. By default is \"FALSE\".
Works only \bold{if} argument "x" is matrix.
}
\item{diag}{
A logical value include the diagonal to the result.
}
}

\value{
Get a lower/upper triangular logical matrix with values \"TRUE\"/\"FALSE\"" or a vector with the values of a lower/upper triangular or the sum of the upper/lower triangular if suma is set \"TRUE\". You can also include diagonal with any operation if argument diag is set to "TRUE".
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

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



#all.equal(lower_tri(c(10,10),diag = TRUE),lower.tri(x,diag = TRUE))

#all.equal(lower_tri(x,diag = TRUE),x[lower.tri(x,diag = TRUE)])

#all.equal(upper_tri(c(10,10),diag = TRUE),upper.tri(x,diag = TRUE))

#all.equal(upper_tri(x,diag = TRUE),x[upper.tri(x,diag = TRUE)])

x<-NULL
}

\keyword{ Lower and Upper triangular of a matrix }

