\name{ Sort a vector coresponding to another }
\alias{sort_cor_vectors}
\alias{sort_index}
\title{
Sort
}

\description{
Sort a vector coresponding to another.
}

\usage{
sort_cor_vectors(x,y)
sort_index(x, descending = FALSE)
}

\arguments{
\item{x}{
A numeric vector.
}
\item{y}{
A numeric vector to help sorting the x.
}
\item{descending}{
A logical value for choosing asceding or descending order.
}
}


\details{
This function implements the R's "sort" function using the C++'s function sort.
}

\value{
Returns the first argument but sorted acording to the second vector.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{sort_mat}, \link{sort_unique}
}
}

\examples{
y <- rnorm(100)
x <- rnorm(100)
b <- sort_cor_vectors(x,y)
b <- sort_index(x)
}

\keyword{ Sort Function }
\keyword{ Sort Indices }