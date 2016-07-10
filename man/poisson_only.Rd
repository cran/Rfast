\name{Many univariate simple poisson regressions}
\alias{poisson_only}
\title{
Many univariate simple poisson regressions
}

\description{
It performs very many univariate simple poisson regressions.
}

\usage{
poisson_only(x, y)
}

\arguments{
\item{x}{
A matrix with the data, where the rows denote the samples (and the two groups) and the columns are the variables. Currently only continuous variables are allowed.
}
\item{y}{
The dependent variable; a numerical vector with many discrete values (count data). 
}
}

\details{
The function is written in C++ and this is why it is very fast. It can accept thousands of predictor variables. It is usefull for univariate screening. 
We provide no p-value correction (such as fdr or q-values); this is up to the user. 
}

\value{
A vector with the deviance of each simple poisson regression model for each predictor variable.
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
\code{\link{univglms}, \link{logistic_only}
}
}

\examples{
## 500 variables, hence 500 univariate regressions are to be fitted
x = matrix( rnorm(100 * 500), ncol = 500 )

y = rpois(100, 10)
system.time( poisson_only(x, y) )

b1 = poisson_only(x, y) 
b1 = as.vector(b1)

b2 = numeric(500)
system.time( for (i in 1:500) b2[i] = glm(y ~ x[, i], poisson)$deviance )

b2 = as.vector(glm(y ~ 1, poisson)$null.dev) - as.vector(b2)
}

\keyword{ Poisson regressions }
\keyword{ generalised linear models } 

