\name{Sum of a matrix }
\alias{matrix.sum}
\title{
Sum of a matrix
}

\description{
Sum of a matrix.
}

\usage{
matrix.sum(x)
}

\arguments{
\item{x}{
A matrix with data.
}
}

%\details{
%
%}

\value{
The sum of the matrix \"x\".
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
\code{\link{rowMins}, \link{rowFalse}, \link{nth}, \link{colrange}, \link{colMedians}, \link{colVars}, \link{sort_mat}, \link{rowTrue}
}
}

\examples{
x <- matrix(runif(100*100),100,100)
f <- matrix.sum(x)
f==sum(x)
}

\keyword{ Replicate in columns }

