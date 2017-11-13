\name{Many analysis of variance tests with a discrete variable}
\alias{poisson.anovas}
\alias{geom.anovas}
\title{
Many analysis of variance tests with a discrete variable
}

\description{
Many analysis of variance tests with a discrete variable.
}

\usage{
poisson.anovas(y, ina, logged = FALSE) 
geom.anovas(y, ina, type = 1, logged = FALSE) 
}

\arguments{
\item{y}{
A numerical matrix with discrete valued data, i.e. counts for the case of the Poisson, or with 0s and 1s for the 
case of the Bernoulli distribution. Each column represents a variable. 
}
\item{ina}{
A numerical vector with discrete numbers starting from 1, i.e. 1, 2, 3, 4,... or a factor variable. 
This is suppose to be a categorical predictor. If you supply a continuous valued vector the function 
will obviously provide wrong results.
}
\item{type}{
This rgument is for the geometric distribution. Type 1 refers to the case where the minimum is zero and type 2 for the case of the minimum being 1. 
}
\item{logged}{
Should the p-values be returned (FALSE) or their logarithm (TRUE)?
}
}

\details{
This is the analysis of variance with Poisson distributed data. What we do is many log-likelihood ratio tests. 
}

\value{
A matrix with two values, the difference in the deviances (test statistic) and the relevant p-value.  
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{g2tests}, \link{poisson.anova}, \link{anova}, \link{poisson_only}, \link{poisson.mle}
}
}

\examples{
ina <- rbinom(500, 3, 0.5) + 1 
## Poisson example
y <- matrix( rpois(500 * 100, 10), ncol= 100 )
system.time(a1 <- poisson.anovas(y, ina) )
y <- NULL
}

\keyword{ Poisson distribution }
\keyword{ geometric distribution }
\keyword{ analysis of variance }
