colvarcomps.mle <- function (x, id, ranef = FALSE, tol = 1e-08, maxiters = 100, 
    parallel = FALSE) {
    if ( Rfast::Var( tabulate(id) ) != 0) {
        res <- .Call("Rfast_colrint_mle", PACKAGE = "Rfast", 
            x, id, ranef, tol, maxiters, parallel)
    }
    else {
        N <- length(id)
        n <- Rfast::sort_unique.length(id)
        d <- N/n
        f <- 1 - 1/d
        mxid <- rowsum(x, id)/d
        mx <- Rfast::colsums(mxid)/n
        ex <- t(t(x) - mx)
        sexid <- rowsum(ex^2, id)
        co <- t((t(mxid) - mx)^2)
        sx2 <- Rfast::colsums(co)
        sml <- Rfast::colsums(sexid - d * co)/N/f
        dml <- sx2/n/sml - 1/d
        tauml <- dml * sml
        poia <- which(dml < 0)
        sml[poia] <- sml[poia] + tauml[poia]
        tauml[poia] <- 0
        sx <- Rfast::colsums(sexid)
        sx2 <- sx2 * d^2
        loglik <- N * log(sml) + n * log1p(d * tauml/sml) + sx/sml - 
            tauml/(sml^2 + d * tauml * sml) * sx2
        loglik <- -0.5 * loglik - N/2 * log(2 * pi)
        info <- cbind(tauml, sml, loglik)
        colnames(info) <- c("sigma_tau", "sigma_errors", "log-likelihood")
        res <- list(info = info)
        if (ranef) {
            com <- t(rowsum(ex, id)/d)
            ranef <- tauml/(tauml + sml/d) * com
            res <- list(info = info, ranef = t(ranef))
        }
    }
    res
}