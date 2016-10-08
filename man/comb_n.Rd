\name{All k possible combinations from n elements}
\alias{comb_n}
\title{
All k possible combinations from n elements
}

\description{
All k possible combinations from n elements.
}

\usage{
comb_n(n, k)
}

\arguments{
\item{n}{
A stirctly (greater than zero) positive integer number.
}
\item{k}{
A positive integer number at most equal to n.
}
}

%\details{
%
%}

\value{
A matrix with k columns and rows equal to the number of possible unique combinations with k elements. 
}

\references{
Nijenhuis, A. and Wilf, H.S. (1978). Combinatorial Algorithms for Computers and Calculators. Academic Press, NY. 
}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
\code{\link{nth}, \link{colMaxs}, \link{colMins}, \link{colrange}
}
}

\examples{
system.time( comb_n(20, 4) )
system.time( combn(20, 4) )
}

\keyword{ Combinatorics }
\keyword{ All possibe combinations }

