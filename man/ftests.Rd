\name{Many F-tests}
\alias{ftests}
\title{
Many F-tests
}

\description{
It performs very many F-tests.
}

\usage{
ftests(x, ina)
}

\arguments{
\item{x}{
A matrix with the data, where the rows denote the samples (and the two groups) and the columns are the variables.
}
\item{ina}{
A numerical vector with 1s, 2s, 3s and so one indicating the two groups. Be careful, the function is desinged to accept numbers greater than zero.
}
}

\details{
The Welch's F-test (without assuming equal variances) is performed.
}

\value{
A matrix with the test statistic and the p-value of each test.
}

\references{
B. L. Welch (1951). On the comparison of several mean values: an alternative approach. Biometrika, 38(3/4), 330-336.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{ttests}, \link{colVars}
}
}

\examples{
## 100000 variables, hence 100000 F-tests will be performed
x = matrix( rnorm(100 * 10000), ncol = 10000 )
## 100 observations in total
ina = rbinom(100, 3, 0.6) + 1   ## independent samples t-test
system.time( ftests(x, ina) ) 
}

\keyword{ F-tests }