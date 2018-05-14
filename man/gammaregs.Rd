\name{Many simple Gamma regressions with a log-link}
\alias{gammaregs}
\title{
Many simple Gamma regressions with a log-link
}

\description{
Many simple Gamma regressions with a log-link.
}

\usage{
gammaregs(y, x, tol = 1e-08, logged = FALSE, maxiters = 100)
}

\arguments{
\item{y}{
The dependent variable, a numerical variable with strictly poisitive, greater than zero, numbers.
}
\item{x}{
A matrix with the indendent variables.
}
\item{tol}{
The tolerance value to terminate the Newton-Raphson algorithm.
}
\item{logged}{
A boolean variable; it will return the logarithm of the pvalue if set to TRUE.
}
\item{maxiters}{
The maximum number of iterations that can take place in each regression.
}
}

\details{
Many simple Gamma regressions with a log-link are fitted.
}

\value{
A matrix with the test statistic values and their relevant (logged) p-values.
}

\references{
McCullagh, Peter, and John A. Nelder. Generalized linear models. CRC press, USA, 2nd edition, 1989. 
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{score.gammaregs}, \link{normlog.regs}, \link{invgauss.regs}, \link{allbetas} }
}

\examples{
y <- abs( rnorm(100) )
x <- matrnorm(100, 100)
a <- gammaregs(y, x)
x <- NULL
}

