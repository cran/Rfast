cauchy.mle <- function (x, tol = 1e-09) {
    n <- length(x)
    m <- Rfast::med(x)
    s <- 0.5 * (nth(x, 3 * n/4) - nth(x, n/4))
    y <- x - m
    y2 <- y^2
    lik1 <- n * log(s) - sum(log(s^2 + y2))
    down <- 1/(s^2 + y2)
    down2 <- down^2
    derm <- 2 * sum(y * down)
    ders <- n/s - 2 * s * sum(down)
    derm2 <- 2 * sum( (y2 - s^2) * down2 )
    ders2 <- -n/s^2 - derm2
    derms <-  -4 * s * sum(y*down2)
    m <- m - ( ders2 * derm - derms * ders ) / (derm2 * ders2 - derms^2)
    s <- s - ( -derms * derm + derm2 * ders ) / (derm2 * ders2 - derms^2)
    y <- x - m
    y2 <- y^2
    lik2 <- n * log(s) - sum( log(s^2 + y2) )
    i <- 2
    while (lik2 - lik1 > tol) {
        i <- i + 1
        lik1 <- lik2
        down <- 1/(s^2 + y2)
        down2 <- down^2
        derm <- 2 * sum(y * down)
        ders <- n/s - 2 * s * sum(down)
        derm2 <- 2 * sum((y2 - s^2)*down2)
        ders2 <- -n/s^2 - derm2
        derms <- -4 * s * sum(y*down2)
        m <- m - ( ders2 * derm - derms * ders ) / (derm2 * ders2 - derms^2)
        s <- s - ( -derms * derm + derm2 * ders ) / (derm2 * ders2 - derms^2)
        y <- x - m
        y2 <- y^2
        lik2 <- n * log(s) - sum( log(s^2 + y2) )
    }
    param <- c(m, s)
    names(param) <- c("location", "scale")
    list(iters = i, loglik = lik2 - n * log(pi), param = param)
}

