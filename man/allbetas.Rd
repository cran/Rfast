\name{Many simple linear regressions coefficients}
\alias{allbetas}
\title{Simple linear regressions coefficients
}

\description{Simple linear regressions coefficients.
}

\usage{
allbetas(y, x, pvalue = FALSE)
}

\arguments{
\item{y}{
A numerical vector with the response variable. If the y contains proportions or percentages, i.e. values between 0 and 1, the logit transformation is applied first and the transformed data are used.
}
\item{x}{
A matrix with the data, where rows denotes the observations and the columns contain the independent variables.
}
\item{pvalue}{
If you want a hypothesis test that each slope (beta coefficient) is equal to zero set this equal to TRUE. It will also produce all the correlations between y and x.
}
}

%\details{
%For every value of \eqn{\alpha} the normal likelihood (see the refernece) is computed. At the end, the plot of the values is constructed.
%}

\value{
A matrix with the constant (alpha) and the slope (beta) for each simple linear regression.
}

%\references{
%Tsagris M.T., Preston S. and Wood A.T.A. (2011). A data-based power transformation for compositional data. In Proceedings of the 4th Compositional Data Analysis Workshop, Girona, Spain.
%}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{correls}, \link{univglms}, \link{cova}, \link{cora}
}
}

\examples{
x <- matrix( rnorm(100 * 10000), ncol = 10000 )
y <- rnorm(100)
r <- cor(y, x)  ## correlation of y with each of the xs
a <- allbetas(y, x)  ## the coefficients of each simple linear regression of y with x
b <- matrix(nrow = 10000, ncol = 2)
for (i in 1:10000) b[i, ] = coef( lm.fit( cbind(1,x[,i]), y ) )

x <- matrix( rnorm(100 * 10000), ncol = 10000 )
y <- rnorm(100)
system.time( allbetas(y, x) )
system.time(  for (i in 1:10000) b[i, ] = coef( lm.fit( cbind(1,x[,i]), y ) )  )

}

\keyword{ Column-wise variances }

