\name{Column-wise range of values of a matrix}
\alias{colrange}
\title{
Column-wise range of values of a matrix.
}

\description{
Column-wise range of values of a matrix.
}

\usage{
colrange(x, cont = TRUE)
}

\arguments{
\item{x}{
A numerical matrix with data.
}
\item{cont}{
If the data are continuous, leave this TRUE and it will return the range of values for each variable (column). If the data are integers, categorical, or if you want 
to find out the number of unique numbers in each column set this to FALSE.
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
\code{\link{colMins}, \link{colMaxs}, \link{rowMins}, \link{rowMaxs}, \link{nth}, \link{colMedians}, \link{colVars}, \link{sort_mat}
}
}

\examples{
x <- matrix( rnorm(1000 * 2000), ncol = 2000 )

system.time( a1 <- colrange(x) )
system.time( a2 <- apply(x, 2, function(x) diff( range(x)) ) )
all.equal(a1, a2)
}

\keyword{ Column-wise ranges}


