univglms2 <- function (y, x, oiko = NULL, logged = FALSE) {
    dm <- dim(x)
    n <- dm[1]
    d <- dm[2]
    if (is.null(oiko)) {
        if (length(sort_unique(y)) == 2) {
            oiko = "binomial"
        }
        else if (sum(round(y) - y) == 0) {
            oiko = "poisson"
        }
        else oiko = "normal"
    }
    if (oiko == "binomial") {
        poia <- Rfast::which.is(x)
        x <- Rfast::data.frame.to_matrix(x)
        if (length(poia) == 0) {
            p <- sum(y)/n
            ini <-  - 2 * ( n * p * log(p) + (n - n * p) * log(1 - p) )
            mod <- Rfast::logistic_only(x, y)
            stat <- ini - mod
            pval <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
        }
        else {
            stat <- numeric(d)
            pval <- numeric(d)
            p <- sum(y)/n
            ini <-  - 2 * ( n * p * log(p) + (n - n * p) * log(1 - p) )
            mod <- Rfast::logistic_only(x[, -poia, drop = FALSE], y)
            stat[-poia] <- ini - mod
            pval[-poia] <- pchisq(stat[-poia], 1, lower.tail = FALSE, log.p = logged)
            x <- cbind(y, x[, poia] - 1)
            dc <- Rfast::colrange(x, cont = FALSE)
            mod <- Rfast::g2tests(x, 2:(length(poia) + 1), 1, dc)
            stat[poia] <- mod$statistic
            pval[poia] <- pchisq(mod$statistic, mod$df, lower.tail = FALSE, log.p = logged)
        }
        result <- cbind(stat, pval)
    }
    else if (oiko == "poisson") {
        poia <- Rfast::which.is(x)
        x <- Rfast::data.frame.to_matrix(x)
        if (length(poia) == 0) {
            m <- sum(y)/n
            ini <- 2 * sum(y * log(y), na.rm = TRUE) - 2 * n * m * log(m)
            mod <- Rfast::poisson_only(x, y)
            stat <- ini - mod
            pval <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
        }
        else {
            stat <- numeric(d)
            pval <- numeric(d)
            m <- sum(y)/n
            ini <- 2 * sum(y * log(y), na.rm = TRUE) - 2 * n * m * log(m)
            mod <- Rfast::poisson_only(x[, -poia, drop = FALSE], y)
            stat[-poia] <- ini - mod
            pval[-poia] <- pchisq(stat[-poia], 1, lower.tail = FALSE, log.p = logged)
            d1 <- numeric(d)
            k <- Rfast::colrange(x[, poia, drop = FALSE], cont = FALSE)
            for (i in poia) {
                ina <- x[, i]
                ni <- tabulate(ina)
                ni <- ni[ni > 0]
                si <- rowsum(y, ina)
                mi <- si/ni
                d1[i] <- sum(si * log(mi))
            }
            d0 <- n * m * log(m)
            stat[poia] <- 2 * d1[poia] - 2 * d0
            pval[poia] <- pchisq(stat[poia], k - 1, lower.tail = FALSE, log.p = logged)
        }
        result <- cbind(stat, pval)
    }
    else if (oiko == "quasipoisson") {
        poia <- Rfast::which.is(x)
        x <- Rfast::data.frame.to_matrix(x)
        if (length(poia) == 0) {
            m <- sum(y)/n
            ini <- 2 * sum(y * log(y), na.rm = TRUE) - 2 * n * m * log(m)
            mod <- Rfast::quasi.poisson_only(x, y)
            stat <- (ini - mod[1, ])/mod[2, ]
            pval <- pf(stat, 1, n - 2, lower.tail = FALSE, log.p = logged)
        }
        else {
            stat <- numeric(d)
            pval <- numeric(d)
            sy <- sum(y)
            m <- sy/n
            ini <- 2 * sum(y * log(y), na.rm = TRUE) - 2 * n * m * log(m)
            if (length(poia) < d) {
                mod <- Rfast::quasi.poisson_only(x[, -poia, drop = FALSE], y)
                stat[-poia] <- (ini - mod[1, ])/mod[2, ]
                pval[-poia] <- pf(stat[-poia], 1, n - 2, lower.tail = FALSE, log.p = logged)
            }
            d0 <- 2 * sy * log(sy/n)
            for (i in poia) {
                ina <- x[, i]
                ni <- tabulate(ina)
                ni <- ni[ni > 0]
                k <- length(ni)
                si <- rowsum(y, ina)
                mi <- si/ni
                d1 <- Rfast::colsums(si * log(mi))
                up <- (2 * d1 - d0)/(k - 1)
                yi2 <- rowsum(y^2, ina)/mi
                phi <- ( Rfast::colsums(yi2) - sy ) / (n - k)
                stat[i] <- up/phi
                pval[i] <- pf(stat[i], k - 1, n - k, lower.tail = FALSE, log.p = logged)
            }
        }
        result <- cbind(stat, pval)
    }
    else if (oiko == "normal") {
        result <- Rfast::regression(x, y, logged = logged)
    }
    colnames(result) <- c("stat", "pvalue")
    result
}
