\name{Sorting a vector}
\alias{Sort}
\title{
Sorting a vector 
}

\description{
Fast sorting a vector.
}

\usage{
Sort(x, descending = FALSE, partial = NULL, stable = FALSE)
}

\arguments{
\item{x}{
A numerical/character vector with data .
}
\item{descending}{
A boolean value (TRUE/FALSE) for sorting the vector in descending order. By default sorts the vector in ascending.
}
\item{partial}{
An index number for sorting partial the vector. Not character vector.
}
\item{stable}{
A boolean value (TRUE/FALSE) for choosing a stable sort algorithm. Stable means that discriminates on the same 
elements. Not character vector.
}
}

\details{
This function uses the sorting algorithm from C++. The implementation is very fast and highly optimised. 
Especially for large data.
}

\value{
The sorted vector.
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
\code{\link{nth}, \link{colnth}, \link{rownth}, \link{sort_cor_vectors}, 
\link{sort_index}, \link{sort_unique}, \link{Round}
}
}

\examples{
x <- rnorm(1000)
system.time( s1 <- Sort(x) )
system.time( s2 <- sort(x) )
all.equal(s1,s2) #true

system.time( s1 <- Sort(x,partial=100) )
system.time( s2 <- sort(x,partial=100) )
all.equal(s1,s2) #true

system.time( s1 <- Sort(x,partial=c(10,100)) )
system.time( s2 <- sort(x,partial=c(10,100)) )
all.equal(s1,s2) #true

system.time( s1 <- Sort(x,stable=TRUE) )
system.time( s2 <- sort(x) )
all.equal(s1,s2) #true

x <- as.character(x)
system.time( s1 <- Sort(x) )
system.time( s2 <- sort(x) )
all.equal(s1,s2) #true
}

\keyword{ Sorting }
\keyword{ Stable Sorting }


