\name{Many univariate simple poisson regressions}
\alias{poisson_only}
\title{
Many univariate simple poisson regressions
}

\description{
It performs very many univariate simple poisson regressions.
}

\usage{
poisson_only(x, y, tol = 1e-09, b_values = FALSE)
}

\arguments{
\item{x}{
A matrix with the data, where the rows denote the samples (and the two groups) and the columns are the variables. Currently only continuous variables are allowed.
}
\item{y}{
The dependent variable; a numerical vector with many discrete values (count data). 
}
\item{tol}{
The tolerance value to terminate the Newton-Raphson algorithm.
}
\item{b_values}{
Do you want the values of the coefficients returned? If yes, set this to TRUE.
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

\seealso{
\code{\link{univglms}, \link{logistic_only}, \link{allbetas}, \link{regression}
}
}

\examples{
## 200 variables, hence 200 univariate regressions are to be fitted
x = matrix( rnorm(100 * 200), ncol = 200 )

y = rpois(100, 10)
system.time( poisson_only(x, y) )

b1 = poisson_only(x, y) 

b2 = numeric(500)
system.time( for (i in 1:200) b2[i] = glm(y ~ x[, i], poisson)$deviance )

all.equal(b1, b2)

b1<-b2<-x<-y<-NULL
}

\keyword{ Poisson regressions }
\keyword{ generalised linear models } 

