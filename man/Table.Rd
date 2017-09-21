\name{Table Creation - Frequency of each value}
\alias{Table}
\title{
Table Creation - Frequency of each value
}

\description{
Table Creation - Frequency of each value.
}

\usage{
Table(x,as.vector = TRUE)
}

\arguments{
\item{x}{
A vector with numeric/character data.
}
\item{as.vector}{
A boolean value for return vector.
}
}

\details{
Like R's "table".
}

\value{
if is.vector is "TRUE" then return a vector with names the discrete values of "x" and values there frequencies. 
Otherwise, a list with 2 fields:
\item{freqs}{
The frequency of each value from vector "x".
}
\item{values}{
The discrete values from vector "x" sorted. 
} 
}

%\references{
%}

\author{
R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{colShuffle}, \link{colVars}, \link{colmeans}, \link{read.directory}
}
}

\examples{
x<-runif(10)
y1<-Table(x)
y2<-as.vector(table(x)) # Neads a lot of time.
all.equal(y1,y2)
y1<-Table(x,FALSE)
all.equal(as.character(y1$values),names(y2))
all.equal(y1$freqs,y2)
}

\keyword{ Table Creation }