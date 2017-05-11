\name{Many score based Beta regressions}
\alias{score.betaregs}
\title{
Many score based Beta regressions
}

\description{
Many score based Beta regressions.
}

\usage{
score.betaregs(y, x, logged = FALSE ) 
}

\arguments{
\item{y}{
A vector with proportions, i.e. values within the (0, 1) interval. 0s and 1s are not allowed. 
}
\item{x}{
A matrix with data, the predictor variables. 
}
\item{logged}{
A boolean variable; it will return the logarithm of the pvalue if set to TRUE.
}
}

\details{
Instead of maximising the log-likelihood via the Newton-Raphson algorithm in order to perform the hypothesis testing that \eqn{\beta_i=0} we use the score test. 
This is dramatcially faster as no model need to be fitted. The first derivative of the log-likelihood is known in closed form and under the null hypothesis the 
fitted values are all equal to the mean of the response variable y. The test is not the same as the likelihood ratio test. It is size correct nonetheless but it is 
a bit less efficient and less powerful. For big sample sizes though (5000 or more) the results are the same. It is also much faster then the classical likelihood ratio test.  
}

\value{
A matrix with two columns, the test statistic and its associated (logged) p-value.  
}

\references{
Ferrari S.L.P. and Cribari-Neto F. (2004). Beta Regression for Modelling Rates and Proportions. Journal of Applied Statistics, 31(7): 799-815.

Cribari-Neto F. and Queiroz M. P. (2014). On testing inference in beta regressions. Journal of Statistical Computation and Simulation, 84(1): 186-203.

Campbell, M.J. (2001). Statistics at Square Two: Understand Modern Statistical Applications in Medicine, pg. 112.
London, BMJ Books. 

}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{score.glms}, \link{univglms}, \link{logistic_only},  \link{poisson_only}, \link{regression}
}
}

\examples{
## 200 variables, hence 200 univariate regressions are to be fitted
x <- matrix( rnorm(100 * 200), ncol = 200 )
y <- rbeta(100, 3, 2)   
system.time( score.betaregs(y, x) )

## x <- matrix( rnorm(100 * 500), ncol = 500 )
## y <- rbeta(100, 3, 2)   
## a <- score.betaregs(y, x)
## hist(a[, 2])
## sum(a < 0.05) / 500  ## estimated type I error 
}

\keyword{ Beta regressions }
\keyword{ score based tests }