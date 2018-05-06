\name{Some summary statistics of a vector for each level of a grouping variable}
\alias{group.var}
\alias{group.all}
\alias{group.any}
\alias{group.mad}
\alias{group.max}
\alias{group.mean}
\alias{group.med}
\alias{group.min}
\alias{group.min_max}
\title{
Some summary statistics of a vector for each level of a grouping variable. 
}

\description{
Some summary statistics of a vector for each level of a grouping variable.
}

\usage{
group.var(x, ina,ina.max = max(ina))
group.all(x, ina,ina.max = max(ina))
group.any(x, ina,ina.max = max(ina))
group.mad(x, ina,method = "median")
group.mean(x, ina,ina.max = max(ina))
group.med(x, ina)
group.min(x, ina,ina.max = max(ina))
group.max(x, ina,ina.min = NULL,ina.max = NULL)
group.min_max(x, ina,ina.max = max(ina))
}

\arguments{
\item{x}{
A numerical vector with data.
}
\item{ina}{
A numerical vector with numbers. Note that zero and negative values are not allowed as this can cause R to run forever or crash.
}
\item{method}{
A character vector with values "median", for median absolute deviation or "mean", for mean absolute deviation. 
}
\item{ina.max}{
Maximum number for vector ina. 
}
\item{ina.min}{
Minimum number for vector ina. 
}
}

\details{
This is the like the "groupcolVars" (or rowsum), but only for vectors. No names are returned, simply a 
vector, similar to "groupcolVars". Note that this command works only for vectors. Median absolute deviation, mean, median, minium, maximum 
are some of the options offered. 
} 

\value{
A vector with the variance, or standard deviation, or mean, or minimum, or maximum, or median, or minimum-maximum of x for each distinct value of ina. 
}

\author{
Manos Papadakis and Michail Tsagris

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com> and Michail Tsagris <mtsagris@yahoo.gr>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{groupcolVars}, \link{group.sum}, \link{colmeans}, \link{colVars}, \link{colMedians}
}
}

\examples{
x <- rgamma(100,1, 4)
ina <- sample(1:5, 100, TRUE)
group.var(x, ina)
#group.mean(x, ina) #run
#group.med(x, ina)  #run
}

\keyword{ Variance }
\keyword{ grouppings }
