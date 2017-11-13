\name{Two sample exponential empirical likelihood test}
\alias{eel.test2}

\title{
Two sample exponential empirical likelihood test
}

\description{
Two sample exponential empirical likelihood test.
}

\usage{
eel.test2(x, y, tol = 1e-09, logged = FALSE)
}

\arguments{
\item{x}{
A numerical vector. 
}
\item{y}{
Another numerical vector.
}
\item{tol}{
The tolerance value to stop the iterations of the Newton-Raphson.
}
\item{logged}{
Should the logarithm of the p-value be returned? TRUE or FALSE.
}
}

\details{
Exponential empirical likelihood is a non parametric method. In this case we use it as the non parametric alternative 
to the t-test. Newton-Raphson is used to maximise the log-likelihood ratio test statistic. In the case of no solution, 
NULL is returned.  
}

\value{
\item{iters}{
The number of iterations required by the Newton-Raphson algorithm. If no covnergence occured this is NULL.
}
\item{info}{
A vector with three elements, the value of the \eqn{\lambda}, the likelihood ratio test statistic and the relevant p-value.
If no convergence occured, the value of the \eqn{\lambda} before is becomes NA, the value of test statistic is \eqn{10^5} and the p-value is 0.
No convergence can be interpreted as rejection of the hypothesis test.
}
\item{p1}{
The estimated probabilities, one for each observation for the first sample. If no covnergence occured this is NULL.
}
\item{p2}{
The estimated probabilities, one for each observation for the second sample. If no covnergence occured this is NULL.
}
}

\references{
Owen A. B. (2001). Empirical likelihood. Chapman and Hall/CRC Press.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{ftests}, \link{ttests},, \link{ttest}
}
}

\examples{
x <- rnorm(100)
y <- rnorm(200)
system.time( eel.test2(x, y) )
x <- rnorm(100)
system.time( eel.test2(x, y) )
x <- rnorm(50)
y <- rexp(100)
system.time( eel.test2(x, y) )
}

\keyword{ Hypothesis testing }
\keyword{ two samples }
\keyword{ non parametric test }
