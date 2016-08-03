\name{Row-wise minimum and maximum}
\alias{rowMins}
\alias{rowMaxs}
\title{
Row-wise minimum and maximum of a matrix.
}

\description{
Row-wise minimum and maximum of a matrix.
}

\usage{
rowMins(x)

rowMaxs(x)
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
\code{\link{colMins}, \link{colMaxs}, \link{nth}, \link{colMedians}, \link{colVars}, \link{sort_mat}
}
}

\examples{
x <- matrix( rnorm(1000 * 2000), ncol = 2000 )

system.time( rowMins(x) )
system.time( apply(x, 1, min) )
s1 = rowMins(x)
s2 = apply(x, 1, min)

system.time( rowMaxs(x) )
system.time( apply(x, 1, max) )
s1 = rowMaxs(x)
s2 = apply(x, 1, max)
}

\keyword{ Row-wise minimum }
\keyword{ row-wise maximum }

