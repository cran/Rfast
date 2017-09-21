\name{Column and row-wise Any}
\alias{colAny}
\alias{rowAny}
\title{
Column and row-wise Any
}

\description{
Column and row-wise Any of a matrix.
}

\usage{
colAny(x)
rowAny(x)
}

\arguments{
\item{x}{
A logical matrix with the data.
}
}

\details{
The functions is written in C++ in order to be as fast as possible.
}

\value{
A vector where item "i" is true if found Any true in column/row "i". Otherwise false.
}

%\references{
%}

\author{
R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{med}, \link{colMedians}, \link{colMeans} (buit-in R function)
}
}

\examples{
x <- matrix(as.logical(rbinom(100*100,1,0.5)),100,100)
system.time( a<-colAny(x) )
system.time( b<-apply(x,2,any) )
all.equal(a,b)

system.time( a<-rowAny(x) )
system.time( b<-apply(x,1,any) )
all.equal(a,b)
}

\keyword{ Column-wise Any }
\keyword{ Row-wise Any }