\name{Many multi-sample tests}
\alias{ftests}
\alias{anovas}
\title{
Many multi-sample tests
}

\description{
Many multi-sample tests.
}

\usage{
ftests(x, ina, logged = FALSE)
anovas(x, ina, logged = FALSE)
}

\arguments{
\item{x}{
A matrix with the data, where the rows denote the samples (and the two groups) and the columns are the variables.
}
\item{ina}{
A numerical vector with 1s, 2s, 3s and so one indicating the two groups. Be careful, the function is desinged to accept numbers greater than zero.
}
\item{logged}{
Should the p-values be returned (FALSE) or their logarithm (TRUE)?
}

}

\details{
The Welch's F-test (without assuming equal variances) is performed with the "ftests" funciton. The "anovas" funciton perform the classical (Fisher's) one-way analysis of variance (ANOVA) which assumes equal variance across the groups. 
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
\code{\link{ttests}
}
}

\examples{
## 1000 variables, hence 1000 F-tests will be performed
x = matrix( rnorm(300 * 1000), ncol = 1000 )
## 300 observations in total
ina = rbinom(300, 3, 0.6) + 1   ## independent samples t-test
system.time( ftests(x, ina) ) 
system.time( anovas(x, ina) )
}

\keyword{ F-tests }
\keyword{ analysis of variance }
