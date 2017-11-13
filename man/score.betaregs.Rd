\name{Many score based regression models}
\alias{score.weibregs}
\alias{score.betaregs}
\alias{score.gammaregs}
\alias{score.expregs}
\alias{score.invgaussregs}
\alias{score.ztpregs}
\alias{score.geomregs}

\title{
Many score based regression models.
}

\description{
Many score based regression models.
}

\usage{
score.weibregs(y, x, logged = FALSE) 
score.betaregs(y, x, logged = FALSE) 
score.gammaregs(y, x, logged = FALSE) 
score.expregs(y, x, logged = FALSE)
score.invgaussregs(y, x, logged = FALSE)
score.ztpregs(y, x, logged = FALSE)
score.geomregs(y, x, logged = FALSE)
}

\arguments{
\item{y}{
A vector with data. For the Weibull, gamma and exponential regressions they must be strictly positive data, 
lifetimes or durations for example. For the beta regression they must be numbers between 0 and 1. For the 
zero truncated Poisson regression (score.ztpregs) they must be integer valued data strictly greater than 0. 
}
\item{x}{
A matrix with data, the predictor variables. 
}
\item{logged}{
A boolean variable; it will return the logarithm of the pvalue if set to TRUE.
}
}

\details{
Instead of maximising the log-likelihood via the Newton-Raphson algorithm in order to perform the hypothesis testing 
that \eqn{\beta_i=0} we use the score test. This is dramatcially faster as no model need to be fitted. The first 
derivative of the log-likelihood is known in closed form and under the null hypothesis the fitted values are all equal 
to the mean of the response variable y. The test is not the same as the likelihood ratio test. It is size correct 
nonetheless but it is a bit less efficient and less powerful. For big sample sizes though (5000 or more) the results 
are the same. You cna try for yourselves and see that even with 500 the results are pretty close. The score test is 
also very faster then the classical likelihood ratio test.  

What we have seen via simulation studies is that it is size correct to large sample sizes, at elast a few thousands. 

}

\value{
A matrix with two columns, the test statistic and its associated p-value.  
}

\references{
McCullagh, Peter, and John A. Nelder. Generalized linear models.  CRC press, USA, 2nd edition, 1989.

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
\code{ \link{score.glms} \link{univglms}, \link{logistic_only},  \link{poisson_only}, \link{regression}
}
}

\examples{
x <- matrnorm(300, 100)
y <- rweibull(300, 2, 3)   
a <- score.weibregs(y, x) 
sum(a[, 2] < 0.05) / 100
x <- NULL
}

\keyword{ Weibull regressions }
\keyword{ beta regressions }
\keyword{ gamma regressions }
\keyword{ exponential regressions }
\keyword{ score based tests }