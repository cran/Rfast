\name{Mean absolute deviation of a vector}
\alias{mad2}
\title{
Mean absolute deviation of a vector
}

\description{
Mean absolute deviation of a vector.
}

\usage{
mad2(x,method = "median")
}

\arguments{
\item{x}{
A numerical vector. 
}
\item{method}{
A character vector with values "median", for median absolute deviation or "mean", for mean absolute deviation. 
}
}

\value{
The mean absolute deviation. 
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> 
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{colMads}, \link{med}, \link{colMedians}
}
}

\examples{
x <- Rnorm(1000)
mad2(x)
mad(x)
}




