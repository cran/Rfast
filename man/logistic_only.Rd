\name{Many univariate simple binary logistic regressions}
\alias{logistic_only}
\title{
Many univariate simple binary logistic regressions
}

\description{
It performs very many univariate simple binary logistic regressions.
}

\usage{
logistic_only(x, y)
}

\arguments{
\item{x}{
A matrix with the data, where the rows denote the samples (and the two groups) and the columns are the variables. Currently only continuous variables are allowed.
}
\item{y}{
The dependent variable; a numerical vector with two values (0  and 1). 
}
}

\details{
The function is written in C++ and this is why it is very fast. It can accept thousands of predictor variables. It is usefull for univariate screening. 
We provide no p-value correction (such as fdr or q-values); this is up to the user. 
}

\value{
A vector with the deviance of each simple binayr logistic regression model for each predictor variable.
}

\references{
McCullagh, Peter, and John A. Nelder. Generalized linear models.  CRC press, USA, 2nd edition, 1989.
}

\author{
Manos Papadakis <papadakm95@gmail.com>

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{allbetas}, \link{correls}, \link{poisson_only}, \link{regression}
}
}

\examples{
## 500 variables, hence 500 univariate regressions are to be fitted
x = matrix( rnorm(100 * 500), ncol = 500 )

## 100 observations in total
y = rbinom(100, 1, 0.6)   ## binary logistic regression
system.time( logistic_only(x, y) )

a1 = logistic_only(x, y)
a1 = as.vector(a1)
 
a2 <- numeric(500)
system.time( for (i in 1:500) a2[i] = glm(y ~ x[, i], binomial)$deviance )

a2 = as.vector( glm(y ~ 1, binomial)$null.dev ) - as.vector(a2)
}

\keyword{ Logistic regressions }
\keyword{ generalised linear models } 

