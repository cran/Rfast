\name{Many 2 sample t-tests}
\alias{ttests}
\title{
Many 2 sample tests t-tests
}

\description{
It performs very many 2 sample t-tests.
}

\usage{
ttests(x, y = NULL, ina, paired = FALSE, logged = FALSE)
}

\arguments{
\item{x}{
A matrix with the data, where the rows denote the samples (and the two groups) and the columns are the variables.
}
\item{y}{
A second matrix with the data of the second group. If this is NULL (default value) then the argument ina must be supplied. Notice that when you supply the two matrices the procedure is two times faster.
} 
\item{ina}{
A numerical vector with 1s and 2s indicating the two groups. Be careful, the function is designed to accept only these two numbers. In addition, if your "y" is NULL, you must specify "ina".
}
\item{paired}{
If the groups are not independent paired t-tests should be performed and this must be TRUE, otherwise, leave it FALSE. In this case, the two groups must have equal smaple sizes, otherwise no test will be performed.
}
\item{logged}{
Should the p-values be returned (FALSE) or their logarithm (TRUE)?
}
}

\details{
If the groups are independent, the Welch's t-test (without assuming equal variances) is performed. Otherwise many paired t-tests are performed. 
}

\value{
A matrix with the test statistic, the degrees of freedom (if the groups are independent) and the p-value (or their logarithm) of each test.
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
\code{\link{ftests}
}
}

\examples{
## 1000 variables, hence 1000 t-tests will be performed
x = matrix( rnorm(100 * 1000), ncol = 1000 )
## 100 observations in total
ina = rbinom(100, 1, 0.6) + 1   ## independent samples t-test
system.time( ttests(x, ina = ina) )
x1 = x[ina == 1, ]
x2 = x[ina == 2, ]
system.time( ttests(x1, x2) )

ina = rep(1:2, each = 50)   ## dependent samples t-test
system.time( ttests(x, ina = ina, paired = TRUE) )
}

\keyword{ t-tests }

