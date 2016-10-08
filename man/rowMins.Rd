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
rowMins(x, value = FALSE)
rowMaxs(x, value = FALSE)
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
%}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
\code{\link{colMins}, \link{colMaxs}, \link{nth}, \link{rowrange} \link{colMedians}, \link{colVars}, \link{sort_mat}
}
}

\examples{
x <- matrix( rnorm(1000 * 2000), ncol = 2000 )

system.time( s1 <- rowMins(x) )
system.time( s1 <- apply(x, 1, min) )

system.time( s1 <- rowMaxs(x) )
system.time( s1 <- apply(x, 1, max) )
}

\keyword{ Row-wise minimum }
\keyword{ row-wise maximum }

