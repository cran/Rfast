\name{Column-wise minimum and maximum (in parallel)}
\alias{colMins}
\alias{colMaxs}
\alias{colMins_parallel}
\alias{colMaxs_parallel}
\title{
Column-wise minimum and maximum of a matrix
}

\description{
Column-wise minimum and maximum of a matrix in parallel.
}

\usage{
colMins(x, value=FALSE)

colMaxs(x, value=FALSE)

colMins_parallel(x, ncores, value = FALSE)

colMaxs_parallel(x, ncores, value = FALSE)
}

\arguments{
\item{x}{
A numerical matrix with data.
}
\item{ncores}{
The number of cores to use.
}
\item{value}{
If the value is TRUE it returns the indices of the minimum/maximum, otherwise it returns the minimum and maximum values.
}
}

%\details{
%
%}

\value{
A vector with the relevant values.
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
\code{\link{rowMins}, \link{rowMaxs}, \link{nth}, \link{colMedians}, \link{colVars}, \link{sort_mat}
}
}

\examples{
x <- matrix( rnorm(1000 * 2000), ncol = 2000 )

system.time( colMins(x) )
system.time( apply(x, 2, min) )
s1 = colMins(x)
s2 = apply(x, 2, min)

system.time( colMaxs(x) )
system.time( apply(x, 2, max) )
s1 = colMaxs(x)
s2 = apply(x, 2, max)

system.time( colMins_parallel(x, ncores = 2) )
system.time( apply(x, 2, min) )
s1 = colMins_parallel(x, ncores = 2)
s2 = apply(x, 2, min)
}

\keyword{ Column-wise minimum }
\keyword{ Column-wise maximum }
\keyword{ parallel computations }

