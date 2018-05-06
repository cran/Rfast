\name{Many simple inverse Gaussian regressions with a log-link}
\alias{invgauss.regs}
\title{
Many simple inverse Gaussian regressions with a log-link
}

\description{
Many simple inverse Gaussian regressions with a log-link.
}

\usage{
invgauss.regs(y, x, tol = 1e-08, logged = FALSE, maxiters = 100)
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
Many simple inverse Gaussian regressions with a log-link are fitted.
}

\value{
A matrix with the test statistic values and their relevant (logged) p-values.
}

\references{
McCullagh, Peter, and John A. Nelder. Generalized linear models. CRC press, USA, 2nd edition, 1989. 

Zakariya Yahya Algamal and Intisar Ibrahim Allyas (2017). Prediction of blood lead level in maternal and fetal 
using generalized linear model. International Journal of Advanced Statistics and Probability, 5(2): 65-69.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{score.invgaussregs}, \link{normlog.regs}, \link{invgauss.reg}, \link{allbetas} }
}

\examples{
y <- abs( rnorm(100) )
x <- matrnorm(100, 100)
a <- invgauss.regs(y, x)
x <- NULL
}

