\name{Column and row-wise Any/All}
\alias{colAny}
\alias{rowAny}
\alias{colAll}
\alias{rowAll}
\title{
Column and row-wise Any
}

\description{
Column and row-wise Any/All of a matrix.
}

\usage{
colAny(x)
rowAny(x)
colAll(x, parallel = FALSE, cores = 0)
rowAll(x, parallel = FALSE, cores = 0)
}

\arguments{
\item{x}{
A logical matrix with the data.
}
\item{parallel}{
Do you want the computations to take place in parallel? The default value is FALSE.
}
\item{cores}{
Number of cores to use for parallelism. Valid only when argument parallel is set to TRUE. 
Default value is 0 and it means the maximum supported cores.
}
}

\details{
The functions is written in C++ in order to be as fast as possible.
}

\value{
A vector where item "i" is true if found Any/All true in column/row "i". Otherwise false.
}


\author{
R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{Median}, \link{colMedians}, \link{colMeans} (buit-in R function)
}
}

\examples{
x <- matrix(as.logical(rbinom(100*100,1,0.5)),100,100)
a<-colAny(x)
#b<-apply(x,2,any)
#all.equal(a,b)

a<-rowAny(x)
#b<-apply(x,1,any)
#all.equal(a,b)

a<-colAll(x)
#b<-apply(x,2,all)
#all.equal(a,b)

a<-b<-x<-NULL
}

\keyword{ Column-wise Any }
\keyword{ Row-wise Any }