\name{Sorting of the columns of a matrix (in parallel)}
\alias{sort_mat}
\alias{sort_mat_parallel}
\title{
Sorting of the columns of a matrix (in parallel).
}

\description{
Fast sorting of the columns of a matrix (in parallel).
}

\usage{
sort_mat(x)

sort_mat_parallel(x, ncores)
}

\arguments{
\item{x}{
A numerical matrix with data.
}
\item{ncores}{
The number of cores to use.
}
}

%\details{
%
%}

\value{
The matrix with its columns independently sorted.
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
\code{\link{nth}, \link{colMaxs}, \link{colMins}
}
}

\examples{
x <- matrix( rnorm(1000 * 20), ncol = 20 )
system.time( sort_mat(x) )
system.time( sort_mat_parallel(x, 2) )
system.time( apply(x, 2, sort)  )
s1 <- sort_mat_parallel(x, 2)
s2 <- apply(x, 2, sort) 
all.equal(as.vector(s1), as.vector(s2))
}

\keyword{ Sorting }
\keyword{ parallel computations }

