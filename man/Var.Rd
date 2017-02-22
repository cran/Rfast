\name{Variance of a vector}
\alias{Var}
\title{
Variance of a vector
}

\description{
Variance of a vector.
}

\usage{
Var(x, std = FALSE)
}

\arguments{
\item{x}{
A vector with data. 
}
\item{std}{
If you want the standard deviation set this to TRUE, otherwise leave it FALSE.
}
}

\details{
This is a faster calculaiton of the usual variance of a matrix. 
}

\value{
The variance of the vector.
}

%\references{
%
%}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
\code{\link{colVars}, \link{cova}
}
}

\examples{
x <- rnorm(10000)
system.time( for (i in 1:1000) Var(x) )
system.time( for (i in 1:1000) var(x) )
}

\keyword{ Variance }
