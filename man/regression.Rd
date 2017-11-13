\name{Many univariate simple linear regressions}
\alias{regression}
\title{
Many univariate simple linear regressions
}

\description{
It performs very many univariate simple linear regressions with or without categorical variables.
}

\usage{
regression(x, y)
}

\arguments{
\item{x}{
A data.frame with the data, where the rows denote the samples (and the two groups) and the columns are the variables. Currently only continuous variables are allowed. A data frame is expected if you have categorical predictor variables. If you only have continuous predictor variables you should the function \code{\link{allbetas}} instead as it is faster. 
}
\item{y}{
The dependent variable; a numerical vector.
}
}

\details{
The function is written in C++ and this is why it is very fast. It can accept thousands of predictor variables. It is usefull for univariate screening. We provide no p-value correction (such as fdr or q-values); this is up to the user. The user must take the F test statistics along with the numertor degrees and calculate the relevant p-values. We provide the degrees of freedom of the numerator in the F test. The degrees of freedom in the denominator are always n - df of the numerator. 
}

\value{
A matrix with two rows. The first row is the F statistic and the second row is the degrees of freedom of the numerator in the F test. That is the number of estimated parameters. For example if some variables are continuous, the number of parameters is 1. In the case of a categorical variable with D distinct values (or levels) the number of parameters is D-1. 
}

\references{
Draper, N.R. and Smith H. (1988). Applied regression analysis. New York, Wiley, 3rd edition.

McCullagh, Peter, and John A. Nelder. Generalized linear models.  CRC press, USA, 2nd edition, 1989.
}

\author{
Manos Papadakis <papadakm95@gmail.com>

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{univglms}, \link{allbetas}, \link{correls}, \link{univglms}, \link{mvbetas}
}
}

\examples{
## 500 variables, hence 500 univariate regressions are to be fitted
x = matrix( rnorm(100 * 200), ncol = 200 )
x = as.data.frame(x)
y = rnorm(100)   
system.time( a1 <- regression(x, y) )

x = matrix( rnorm(150 * 200), ncol = 200 )
x = as.data.frame(x)
y = rnorm(150)   
for (i in 1:200) x[, i] <- iris[, 5]
system.time( a <- regression(x, y) )
a[, 1:5]
summary(lm(y ~ x[, 1]) )  ## check the F-test

x<-y<-a<-NULL
}

\keyword{ Simple linear regressions }
\keyword{ categorical variables }

