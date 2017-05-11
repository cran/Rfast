\name{Correlation between pairs of variables}
\alias{corpairs}
\title{
Correlation between pairs of variables
}

\description{
Correlations between pairs of variables.
}

\usage{
corpairs(x, y, rho = NULL, logged = FALSE) 
}

\arguments{
\item{x}{
A matrix with real valued data.
}
\item{y}{
A matrix with real valued data whose dimensions match those of x.
}
\item{rho}{
This can be a vector of assumed correlations (equal to the number of variables or the columns of x or y) to 
be tested. If this is not the case, leave it NULL and only the correlations will be returned.
}
\item{logged}{
Should the p-values be returned (FALSE) or their logarithm (TRUE)? This is taken into account only if "rho" is 
a vector. 
}
}

\details{
The paired correlations are calculated. For each column of the matrices x and y the correlation between them is 
calculated.
}

\value{
A vector of correlations in the case of "rho" being NULL, or a matrix with two extra columns, the test 
statistic and the (logged) p-value.
}

\references{
Lambert Diane (1992). Zero-Inflated Poisson Regression, with an Application to Defects in 
Manufacturing. Technometrics. 34(1):1-14.

Johnson Norman L., Kotz Samuel and Kemp Adrienne W. (1992). Univariate Discrete 
Distributions (2nd ed.). Wiley

Cohen, A. Clifford (1960). Estimating parameters in a conditional Poisson distribution. Biometrics. 16:203-211.

Johnson, Norman L. Kemp, Adrianne W. Kotz, Samuel (2005). Univariate Discrete Distributions (third edition). 
Hoboken, NJ: Wiley-Interscience.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{correls}, \link{allbetas}, \link{mvbetas}
}
}

\examples{
x <- matrix( rnorm(100 * 100), ncol = 100 )
y <- matrix( rnorm(100 * 100), ncol = 100 )
system.time( corpairs(x, y) )
a <- corpairs(x, y)
}

\keyword{ Correlations }
