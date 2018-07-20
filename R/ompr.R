ompr <- function (y, x, method = "BIC", tol = 2) {
    dm <- dim(x)
    d <- dm[2]
    n <- dm[1]
    ind <- 1:d
    m <- sum(y)/n
    y <- y - m
    x <- Rfast::eachrow(x, sqrt(Rfast::colsums(x^2)), oper = "/")
    if (method == "sse") {
        rho <- Rfast::Var(y) * (n - 1)
        r <- cov(y, x)
        epe <- which( is.na(r) )
        ind[epe] <- 0
        sel <- which.max(abs(r))
        sela <- sel
        res <- .lm.fit(x[, sel, drop = FALSE], y)$residuals
        rho[2] <- sum(res^2)
        ind[sel] <- 0
        r[sel] <- 0
        i <- 2
        while ((rho[i - 1] - rho[i])/(rho[i - 1]) > tol & i < 
            n) {
            i <- i + 1
            r[sela] <- 0
            r[ind] <- Rfast::eachcol.apply(x, res, indices = ind[ind > 
                0], oper = "*", apply = "sum")
            sel <- which.max(abs(r))
            sela <- c(sela, sel)
            res <- .lm.fit(x[, sela], y)$residuals
            rho[i] <- sum(res^2)
            ind[sela] <- 0
        }
        len <- length(sela)
        info <- cbind(c(0, sela[-len]), rho[1:len])
        colnames(info) <- c("Vars", "|sse|")
    }
    else if (method == "BIC") {
        con <- n * log(2 * pi) + n
        rho <- n * log(Var(y) * (n - 1)/n) + 2 * log(n)
        r <- cov(y, x)
        epe <- which( is.na(r) )
        ind[epe] <- 0
        sel <- which.max(abs(r))
        sela <- sel
        res <- .lm.fit(x[, sel, drop = FALSE], y)$residuals
        rho[2] <- n * log(sum(res^2)/n) + 3 * log(n)
        ind[sel] <- 0
        r[sel] <- 0
        i <- 2
        while (rho[i - 1] - rho[i] > tol & i < n) {
            i <- i + 1
            r[sela] <- 0
            r[ind] <- Rfast::eachcol.apply(x, res, indices = ind[ind > 
                0], oper = "*", apply = "sum")
            sel <- which.max(abs(r))
            sela <- c(sela, sel)
            res <- .lm.fit(x[, sela], y)$residuals
            rho[i] <- n * log(sum(res^2)/n) + (i + 1) * log(n)
            ind[sela] <- 0
        }
        len <- length(sela)
        info <- cbind(c(0, sela[-len]), rho[1:len] + con)
        colnames(info) <- c("Vars", "BIC")
    }
    else if (method == "ar2") {
        down <- Var(y) * (n - 1)
        rho <- 0
        r <- cov(y, x)
        epe <- which( is.na(r) )
        ind[epe] <- 0
        sel <- which.max(abs(r))
        sela <- sel
        res <- .lm.fit(x[, sel, drop = FALSE], y)$residuals
        r2 <- 1 - sum(res^2)/down
        rho[2] <- 1 - (1 - r2) * (n - 1)/(n - 2)
        ind[sel] <- 0
        r[sel] <- 0
        i <- 2
        while (rho[i] - rho[i - 1] > tol & i < n) {
            i <- i + 1
            r[sela] <- 0
            r[ind] <- Rfast::eachcol.apply(x, res, indices = ind[ind > 
                0], oper = "*", apply = "sum")
            sel <- which.max(abs(r))
            sela <- c(sela, sel)
            res <- .lm.fit(x[, sela], y)$residuals
            r2 <- 1 - sum(res^2)/down
            rho[i] <- 1 - (1 - r2) * (n - 1)/(n - i - 1)
            ind[sela] <- 0
        }
        len <- length(sela)
        info <- cbind(c(0, sela[-len]), rho[1:len])
        colnames(info) <- c("Vars", "adjusted R2")
    }
    info
}
