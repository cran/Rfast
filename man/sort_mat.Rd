\name{Sorting of the columns of a matrix}
\alias{sort_mat}
\title{
Sorting of the columns of a matrix 
}

\description{
Fast sorting of the columns of a matrix.
}

\usage{
sort_mat(x)
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
\code{\link{nth}, \link{colMaxs}, \link{colMins}, \link{colrange}, \link{sort_cor_vectors}, 
\link{sort_index}, \link{sort_unique}
}
}

\examples{
x <- matrix( rnorm(1000 * 500), ncol = 500 )
system.time( s1 <- sort_mat(x) )
system.time( s2 <- apply(x, 2, sort)  )
all.equal(as.vector(s1), as.vector(s2))
}

\keyword{ Sorting }


