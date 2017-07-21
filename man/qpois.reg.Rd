\name{Quasi Poisson regression}
\alias{qpois.reg}
\title{
Quasi Poisson regression
}

\description{
Quasi Poisson regression.
}

\usage{
qpois.reg(x, y, full = FALSE, tol = 1e-09) 
}

\arguments{
\item{x}{
The predictor variables. This can be a matrix or a data frame. 
}
\item{y}{
A numerical vector with positive discrete data. 
}
\item{full}{
If this is FALSE, the coefficients, the deviance and the estimated phi parameter will be returned only. 
If this is TRUE, more information is returned. 
}
\item{tol}{
The tolerance value to terminate the Newton-Raphson algorithm. This is set to \eqn{10^{-9}} by default.
}
}

\details{
We are using the Newton-Raphson, but unlike R's built-in function "glm" we do no checks and no extra calculations, 
or whatever. Simply the model, unless the user requests for the Wald tests of the coefficients.  
}

\value{
When full is FALSE a list including:
\item{be}{
The regression coefficients.
}
\item{devi}{
The deviance of the model.
}
\item{varb}{
The covariance matrix of the beta coefficients.
}
\item{phi}{
The phi parameter, the estimate of dispersion.
}
When full is TRUE, the additional item is:
\item{info}{
The regression coefficients, their standard error, their Wald test statistic and their p-value. 
}
}

\references{
McCullagh, Peter, and John A. Nelder. Generalized linear models. CRC press, USA, 2nd edition, 1989.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{prop.reg} \link{univglms}, \link{score.glms}, \link{poisson_only}
}
}

\examples{
y <- rnbinom(100, 10, 0.6)
x <- matrix(rnorm(100*3), ncol = 3)
mod1 <- glm(y ~ x, quasipoisson)
summary(mod1)
qpois.reg(x, y, full = TRUE)
}

\keyword{ GLMs }
\keyword{ Quasi Poisson regression }
