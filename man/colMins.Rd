\name{Column-wise minimum and maximum }
\alias{colMins}
\alias{colMaxs}
\title{
Column-wise minimum and maximum of a matrix
}

\description{
Column-wise minimum and maximum of a matrix in parallel.
}

\usage{
colMins(x, value=FALSE)

colMaxs(x, value=FALSE)
}

\arguments{
\item{x}{
A numerical matrix with data.
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

system.time( s1 <- colMins(x) )
system.time( s2 <- apply(x, 2, min) )

system.time( s1 <- colMaxs(x) )
system.time( s2 <- apply(x, 2, max) )
}

\keyword{ Column-wise minimum }
\keyword{ Column-wise maximum }
\keyword{ parallel computations }

