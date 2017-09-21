\name{Column and row-wise nth smallest value of a matrix/vector}
\alias{colnth}
\alias{rownth}
\alias{nth}
\title{
Column and row-wise nth smallest value of a matrix/vector
}

\description{
Column and row-wise nth smallest value of a matrix/vector.
}

\usage{
colnth(x,elems, parallel = FALSE)
rownth(x,elems, parallel = FALSE)
nth(x, k)
}

\arguments{
\item{x}{
A matrix with the data.
}
\item{elems}{
An integer vector with the kth smallest number to be returned for each column/row.
}
\item{k}{
The kth smallest number to be returned. 
}
\item{parallel}{
Do you want to do it in parallel in C++? TRUE or FALSE Only for col-row wise.
}
}

\details{
The functions is written in C++ in order to be as fast as possible.
}

\value{
For "colnth" , "rownth": A vector with the column/row nth

For "nth": The nth value.
}

%\references{
%}

\author{
Manos Papadakis <papadakm95@gmail.com>

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{med}, \link{colMedians}, \link{colMeans} (buit-in R function)
}
}

\examples{
x <- matrix( rnorm(100 * 100), ncol = 100 )
elems <- sample(1:100,100,TRUE)
system.time( colnth(x,elems) )
system.time( rownth(x,elems) )
x <- rnorm(1000)
nth(x, 500)
sort(x)[500]
}

\keyword{ Column-wise nth }
\keyword{ Row-wise nth }
\keyword{ nth elements }