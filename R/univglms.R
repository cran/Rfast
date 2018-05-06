univglms <- function (y, x, oiko = NULL, logged = FALSE) {
    dm <- dim(x)
    n <- dm[1]
    d <- dm[2]
    if (is.null(oiko)) {
        if ( length( Rfast::sort_unique(y) ) == 2 ) {
            oiko = "binomial"
        }
        else if ( sum( Rfast::Round(y) - y) == 0 ) {
            oiko = "poisson"
        }
        else oiko = "normal"
    }
    if (oiko == "binomial") {
        p <- sum(y)/n
        ini <-  - 2 * ( n * p * log(p) + (n - n * p) * log(1 - p) )
        mod <- Rfast::logistic_only(x, y)
        stat <- ini - mod
        pval <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
    }
    else if (oiko == "poisson") {
        m <- sum(y)/n
        ini <- 2 * sum(y * log(y), na.rm = TRUE) - 2 * n * m * log(m)
        mod <- Rfast::poisson_only(x, y)
        stat <- ini - mod
        pval <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
    }
    else if (oiko == "normal") {
        rho <- as.vector(cor(y, x))
        sqdof <- sqrt(n - 2)
        stat <- rho * sqdof/sqrt(1 - rho^2)
        if (logged) {
            pval <- log(2) + pt(abs(stat), n - 2, lower.tail = FALSE, log.p = TRUE)
        }
        else pval <- 2 * pt(abs(stat), n - 2, lower.tail = FALSE)
    }
    else if (oiko == "quasipoisson") {
        m <- sum(y)/n
        ini <- 2 * sum(y * log(y), na.rm = TRUE) - 2 * n * m * log(m)
        mod <- Rfast::quasi.poisson_only(x, y)
        stat <- (ini - mod[1, ])/mod[2, ]
        pval <- pf(stat, 1, n - 2, lower.tail = FALSE, log.p = logged)
    }
    result <- cbind(stat, pval)
    colnames(result) <- c("stat", "pvalue")
    if (is.null(colnames(x))) {
        rownames(result) <- paste("Var", 1:d, sep = "")
    }
    else row.names(result) <- colnames(x)
    result
}
