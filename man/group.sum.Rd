\name{Sums of a vector for each level of a grouping variable}
\alias{group.sum}
\title{
Sums of a vector for each level of a grouping variable
}

\description{
Sums of a vector for each level of a grouping variable.
}

\usage{
group.sum(x, ina,ina.max = NULL,ina.min = NULL)
}

\arguments{
\item{x}{
A numerical vector whose sums are to be calculated for each value of "ina".
}
\item{ina}{
A numerical vector with numbers. Note that zero and negative values are not allowed as this can cause R to run forever or crash.
}
\item{ina.min}{
Minimum \bold{INTEGER} number for vector ina as vector or NULL. 
}
\item{ina.max}{
Maximum \bold{INTEGER} number for vector ina as vector or NULL. 
}
}

\details{
This is the rowsum, but only for vectors. No names are returned, simply a matrix with one column, just like rowsum. Note that this command works 
only for vectors.
}

\value{
A matrix with one column, the sum of x for each distinct value of ina.
}

%\references{
%Tsamardinos I., Aliferis C. F. and Statnikov, A. (2003). Time and sample efficient discovery of Markov blankets and direct causal relations. 
%In Proceedings of the 9th ACM SIGKDD international conference on Knowledge discovery and data mining (pp. 673-678). 
%}

\author{
We found the C++ code (written by Francois Romain) in
http://stackoverflow.com/questions/16975034/rcpp-equivalent-for-rowsum

Manos Papadakis then added it in Rfast. 

R Documentation: Michail Tsagris <mtsagris@yahoo.gr>.
}

\seealso{
\code{\link{colmeans}, \link{colVars}, \link{Var}, \link{med}}
}

\examples{
x <- rnorm(1000)
ina <- sample(1:5, 1000, replace = TRUE)
a1 <- group.sum(x, ina)
a2 <- rowsum(x, ina)
}

\keyword{ Grouped sums }
