\name{Many univariate generalised linear models}
\alias{univglms}
\title{
Many univariate generalised linear regressions
}

\description{
It performs very many univariate generalised linear regressions.
}

\usage{
univglms(y, x, oiko = NULL, logged = FALSE)
}

\arguments{
\item{y}{
The dependent variable. It can be a factor or a numerical variable with two values only (binary logistic regression), a discrete valued vector (count data)
corresponding to a poisson regression or a numerical vector with continuous values (normal regression). If it contains percentages or proportions (values between 0 and 1)
they are transformed in \eqn{R} using \eqn{log{y}/log{(1-y)}} and linear regression is applied. 
}
\item{x}{
A matrix with the data, where the rows denote the samples (and the two groups) and the columns are the variables. Currently only continuous variables are allowed.
}
\item{oiko}{
This can be either "normal", "poisson" or "binomial". If you are not sure leave it NULL and the function will check internally. However, you might have discrete data (e.g. years of age)
and want to perform many simple linear regressions. In this case you should specify the family. 
}
\item{logged}{
A boolean variable; it will return the logarithm of the pvalue if set to TRUE.
}
}

\details{
If you specify no family of distributions the function internally checkes the type of your data and decides on the type of regression to perform. 
The function is written in C++ and this is why it is very fast. It can accept thousands of predictor variables. It is usefull for univariate screening. 
We provide no p-value correction (such as fdr or q-values); this is up to the user. 
}

\value{
A matrix with the test statistic and the p-value for each predictor variable.
}

\references{
Draper, N.R. and Smith H. (1988). Applied regression analysis. New York, Wiley, 3rd edition.

McCullagh, Peter, and John A. Nelder. Generalized linear models.  CRC press, USA, 2nd edition, 1989.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{logistic_only}, \link{poisson_only}, \link{allbetas}, \link{correls}, \link{regression}
}
}

\examples{
## 500 variables, hence 200 univariate regressions are to be fitted
x = matrix( rnorm(100 * 200), ncol = 200 )

## 100 observations in total
y = rbinom(100, 1, 0.6)   ## binary logistic regression
system.time( univglms(y, x) )

a1 = univglms(y, x) 

a2 <- numeric(200)
system.time( for (i in 1:200) a2[i] = glm(y ~ x[, i], binomial)$deviance )

a2 = glm(y ~ 1, binomial)$null.dev - a2

### poisson regression
y = rpois(100, 10)
system.time(  univglms(y, x) )

b1 = univglms(y, x) 
b2 <- numeric(200)
system.time( for (i in 1:200) b2[i] = glm(y ~ x[, i], poisson)$deviance )

b2 = glm(y ~ 1, poisson)$null.dev - b2

}

\keyword{ Univariate regressions }
\keyword{ generalised linear models } 

