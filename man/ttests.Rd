\name{Many t-tests}
\alias{ttests}
\title{
Many t-tests
}

\description{
It performs very many t-tests.
}

\usage{
ttests(x, ina, paired = FALSE)
}

\arguments{
\item{x}{
A matrix with the data, where the rows denote the samples (and the two groups) and the columns are the variables.
}
\item{ina}{
A numerical vector with 1s and 2s indicating the two groups. Be careful, the funct?on is designed to accept only these two numbers.
}
\item{paired}{
If the groups are not independent paired t-tests should be performed and this must be TRUE, otherwise, leave it FALSE. In this case, the two groups must have equal smaple sizes, otherwise no test will be performed.
}
}

\details{
If the groups are independent, the Welch's t-test (without assuming equal variances) is performed. Otherwise many paired t-tests are performed.
}

\value{
A matrix with the test statistic, the degrees of freedom (if the groups are independent) and the p-value of each test.
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
\code{\link{colVars}
}
}

\examples{
## 100000 variables, hence 100000 t-tests will be performed
x = matrix( rnorm(100 * 100000), ncol = 100000 )
## 100 observations in total
ina = rbinom(100, 1, 0.6) + 1   ## independent samples t-test
system.time( ttests(x, ina) )

ina = rep(1:2, each = 50)   ## dependent samples t-test
system.time( ttests(x, ina, paired = TRUE) )
}

\keyword{ t-tests }

