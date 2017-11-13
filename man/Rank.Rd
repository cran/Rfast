\name{Ranks of the values of a vector}
\alias{Rank}
\title{
Ranks of the values of a vector
}

\description{
Ranks of the values of a vector.
}

\usage{
Rank(x,method = "average",descending = FALSE,stable = FALSE)
}

\arguments{
\item{x}{
A numerical vector with data.
}
\item{method}{
a character string for choosing method. Must be one of "average", "min", "max", "first".
}
\item{descending}{
A boolean value (TRUE/FALSE) for sorting the vector in descending order. By default sorts the vector in ascending.
}
\item{stable}{
A boolean value (TRUE/FALSE) for choosing a stable sort algorithm. Stable means that discriminates on the same elements. Only for the method "first".
}
}

\details{
The ranks of the values are returned, the same job as "rank". If you want you can choose descending/ascending order for all methods. Only for method "first" you can choose for stable sorting but choose it if you have many duplicates values.
}

\value{
A vector with the ranks of the values.
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

\seealso{
\code{\link{colRanks}, \link{correls}
}
}

\examples{
x <- rnorm(100)
a1 <- Rank(x)
a2 <- rank(x)
}


