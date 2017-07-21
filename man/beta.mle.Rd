\name{MLE of distributions defined in the (0, 1) interval}
\alias{beta.mle}
\alias{ibeta.mle}
\alias{logitnorm.mle}
\alias{hsecant01.mle}
\title{
MLE of distributions defined in the (0, 1) interval
}

\description{
MLE of distributions defined in the (0, 1) interval.
}

\usage{
beta.mle(x, tol = 1e-09)
ibeta.mle(x, tol = 1e-09)
logitnorm.mle(x)
hsecant01.mle(x, tol = 1e-09)
}

\arguments{
\item{x}{
A numerical vector with proportions, i.e. numbers in (0, 1) (zeros and ones are not allowed).
}
\item{tol}{
The tolerance level up to which the maximisation stops.
}
}

\details{
Maximum likelihood estimation of the parameters of the beta  distribution is performed via Newton-Raphson. The distributions and hence the functions 
does not accept zeros. \"logitnorm.mle\" fits the logistic normal, hence no nwewton-Raphson is required and the \"hypersecant01.mle\" uses the golden ratio 
search as is it faster than the Newton-Raphson (less calculations)
}

\value{
A list including:
\item{iters}{
The number of iterations required by the Newton-Raphson.
}
\item{loglik}{
The value of the log-likelihood.
}
\item{param}{
The estimated parameters. In the case of \"hypersecant01.mle\" this is called \"theta\" as there is only one parameter.
}
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{diri.nr2}, 
}
}

\examples{
x <- rbeta(1000, 1, 4)
system.time( for(i in 1:1000) beta.mle(x) )
beta.mle(x)
ibeta.mle(x)

x <- runif(1000)
hsecant01.mle(x)
logitnorm.mle(x)
ibeta.mle(x)

x <- rbeta(1000, 2, 5)
x[sample(1:1000, 50)] <- 0
ibeta.mle(x)

## Newton Raphson for the beta distribution
## We use the diri.nr2 function though

# beta.mle <- function (x, tol = 1e-09) {
#   n <- length(x)
#   sly1 <- sum(log(x))
#   sly2 <- sum(log(1 - x))
#   sy <- sum(x)
#   sy2 <- sum(x^2)
#   iniphi <- (sy - sy2)/(sy2 - sy^2/n) * (n - 1)/n
#   a <- sum(x) * iniphi/n
#   b <- iniphi - a
#   phi <- a + b
#   derab <- n * trigamma(phi)
#   dera <- n * digamma(phi) - n * digamma(a) + sly1
#   dera2 <- n * trigamma(phi) - n * trigamma(a)
#   derb <- n * digamma(phi) - n * digamma(b) + sly2
#   derb2 <- n * trigamma(phi) - n * trigamma(b)
#   aold <- c(a, b)
#   anew <- aold - c(derb2 * dera - derab * derb, -derab * dera + 
#                      dera2 * derb)/(dera2 * derb2 - derab^2)
#   i <- 2
#   while (sum(abs(anew - aold)) > tol) {
#     i <- i + 1
#     aold <- anew
#     a <- anew[1]
#     b <- anew[2]
#     phi <- a + b
#     derab <- n * trigamma(phi)
#     dera <- n * digamma(phi) - n * digamma(a) + sly1
#     dera2 <- n * trigamma(phi) - n * trigamma(a)
#     derb <- n * digamma(phi) - n * digamma(b) + sly2
#     derb2 <- n * trigamma(phi) - n * trigamma(b)
#     anew <- aold - c(derb2 * dera - derab * derb, -derab * 
#                        dera + dera2 * derb)/(dera2 * derb2 - derab^2)
#   }
#   a <- anew[1]
#   b <- anew[2]
#   loglik <- -n * lbeta(a, b) + (a - 1) * sly1 + (b - 1) * sly2
#   names(anew) <- c(\"alpha\", \"beta\")
#   list(iters = i, loglik = loglik, param = anew)
# }



}

\keyword{ Beta distribution }
\keyword{ logistic normal distribution }
\keyword{ inflated beta distribution }
\keyword{ hypersecant distribution for proportions }
\keyword{ maximum likelihood estimation }
\keyword{ Newton-Raphson }
\keyword{ proportions }
