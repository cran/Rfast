cauchy.mle <- function (x, tol = 1e-09) {
    n <- length(x)
    m <- med(x)
    s <- 0.5 * (nth(x, 3 * n/4) - nth(x, n/4))
    y <- x - m
    y2 <- y^2
    lik1 <- n * log(s/pi) - sum( log(s^2 + y2) )
    down <- s^2 + y2
    down2 <- down^2
    deri <- c(2 * sum(y/down), n/s - 2 * s * sum(1/down))
    derm2 <- 2 * sum((y2 - s^2)/down2)
    ders2 <- -n/s^2 - derm2
    derms <- -4 * s * sum(y/down2)
    pa <- c(m, s)
    pa <- pa - c(ders2 * deri[1] - derms * deri[2], -derms * 
        deri[1] + derm2 * deri[2]) / (derm2 * ders2 - derms^2)
    lik2 <- n * log(pa[2]/pi) - sum( log(pa[2]^2 + y2) )
    i <- 2
    while ( lik2 - lik1 > tol) {
        i <- i + 1
        lik1 <- lik2
        m <- pa[1]
        s <- pa[2]
        y <- x - m
        y2 <- y^2
        down <- s^2 + y2
        down2 <- down^2
        deri <- c(2 * sum(y/down), n/s - 2 * s * sum(1/down))
        derm2 <- 2 * sum( (y2 - s^2)/down2 )
        ders2 <- -n/s^2 - derm2
        derms <- -4 * s * sum(y/down2)
        pa <- pa - c(ders2 * deri[1] - derms * deri[2], -derms * 
            deri[1] + derm2 * deri[2]) / (derm2 * ders2 - derms^2)
        lik2 <- n * log(pa[2]/pi) - sum( log(pa[2]^2 + y2) )
    }
    names(pa) <- c("location", "scale")
    lik <- lik2 
    list(iters = i, loglik = lik, param = pa)
}
