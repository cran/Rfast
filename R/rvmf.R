rvmf <- function (n, mu, k) {
    rotation <- function(a, b) {
        p <- length(a)
        ab <- sum(a * b)
        ca <- a - b * ab
        ca <- ca/sqrt(sum(ca^2))
        A <- b %*% t(ca)
        A <- A - t(A)
        theta <- acos(ab)
        diag(p) + sin(theta) * A + (cos(theta) - 1) * (b %*% 
            t(b) + ca %*% t(ca))
    }
    d <- length(mu)
    if (k > 0) {
        mu <- mu/sqrt(sum(mu^2))
        ini <- c(numeric(d - 1), 1)
        d1 <- d - 1
        v1 <- Rfast::matrnorm(n, d1)  ##  matrix( RcppZiggurat::zrnorm(n * d1), ncol = d1 )
        v <- v1 / sqrt( Rfast::rowsums(v1^2) )
        b <- (-2 * k + sqrt(4 * k^2 + d1^2))/d1
        x0 <- (1 - b)/(1 + b)
        m <- 0.5 * d1
        ca <- k * x0 + (d - 1) * log(1 - x0^2)
        w <- .Call("Rfast_rvmf_h", PACKAGE = "Rfast", n, ca, 
            d1, x0, m, k, b)
        S <- cbind(sqrt(1 - w^2) * v, w)
        A <- rotation(ini, mu)
        x <- tcrossprod(S, A)
    }
    else {
        x1 <- Rfast::matrnorm(n, d)  ## matrix( RcppZiggurat::zrnorm(n * d), ncol = d )
        x <- x1/sqrt(Rfast::rowsums(x1^2))
    }
    x
}



# rvmf <- function(n, mu, k) {
#   ## n is the sample size
#   ## mu is the mean direction and
#   ## k is the concentration parameter
#   ## n is the sample size
#   d <- length(mu)  ## the dimensions
#   if (k > 0) {
#     mu <- mu / sqrt( sum(mu^2) )  ## the mean direction
#     ini <- c(numeric(d - 1), 1)  ## mean direction is set to (0, ..., 0, 1)
#     d1 <- d - 1
#     v1 <- matrix( RcppZiggurat::zrnorm(n * d1), ncol = d1)
#     v <- v1 / sqrt( rowsums(v1^2) )
#     b <- ( -2 * k + sqrt(4 * k^2 + d1^2) ) / d1
#     x0 <- (1 - b)/(1 + b)
#     w <- numeric(n)
#     m <- 0.5 * d1
#     ca <- k * x0 + (d - 1) * log(1 - x0^2)
#     
#     for (i in 1:n) {
#       ta <-  -1000
#       u <- 1
#       while ( ta - ca < log(u) ) {
#         z <- rbeta(1, m, m)
#         u <- runif(1)
#         w[i] <- ( 1 - (1 + b) * z ) / ( 1 - (1 - b) * z )
#         ta <- k * w[i] + d1 * log(1 - x0 * w[i])
#       }
#     }
#     S <- cbind(v, w)
#     A <- rotation(ini, mu)  ## calculate the rotation matrix
#     ## in order to rotate the initial mean direction from ini to mu
#     x <- tcrossprod(S, A)  ## the x has direction mu
#   } else {  ## uniform distribution
#     ## requires MASS if k = 0
#     x1 <- matrix( RcppZiggurat::zrnorm(n * d), ncol = d )
#     x <- x1 / sqrt( Rfast::rowsums(x1^2) )
#   }
#   x
# }