\name{Many simple Gaussian regressions with a log-link}
\alias{normlog.regs}
\title{
Many simple Gaussian regressions with a log-link
}

\description{
Many simple Gaussian regressions with a log-link.
}

\usage{
normlog.regs(y, x, tol = 1e-08, logged = FALSE, parallel = FALSE, maxiters = 100)
}

\arguments{
\item{y}{
The dependent variable, a numerical variable with non negative numbers.
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
\item{parallel}{
Do you want this to be executed in parallel or not. The parallel takes place in C++, therefore you do not have the option to set the number of cores.
}
\item{maxiters}{
The maximum number of iterations that can take place in each regression.
}
}

\details{
Many simple Gaussian regressions with a log-link are fitted.
}

\value{
A matrix with the test statistic values and their relevant (logged) p-values.
}

\author{
Stefanos Fafalios

R implementation and documentation: Stefanos Fafalios <stefanosfafalios@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{normlog.reg}, \link{score.glms}, \link{prop.regs}, \link{allbetas} }
}

\examples{
y <- abs( rnorm(100) )
x <- matrnorm(100, 100)
a <- normlog.regs(y, x)
b <- glm(y ~ x[, 1], family = gaussian(log) )
anova(b, test= "F")
a[1, ]
x <- NULL
}

