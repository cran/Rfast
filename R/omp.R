omp <- function (y, x, tol = qchisq(0.95, 1) + log(length(y)), type = "logistic") {
    tic <- proc.time()
    dm <- dim(x)
    d <- dm[2]
    n <- dm[1]
    ind <- 1:d
    x <- Rfast::standardise(x)
    phi <- NULL
    if (type == "logistic") {
        p <- sum(y)/n
        rho <- -2 * (n * p * log(p) + (n - n * p) * log(1 - p))
        ela <- as.vector( cov(y, x) )
        sel <- which.max( abs(ela) )
        sela <- sel
        names(sela) <- NULL
        options(warn = -1)
        mod <- Rfast::glm_logistic(x[, sel], y)
        est <- exp(-mod$be[1] - x[, sel] * mod$be[2])
        res <- y - 1/(1 + est)
        rho[2] <- mod$devi
        ind[sel] <- 0
        i <- 2
        while ( (rho[i - 1] - rho[i]) > tol ) {
            r <- numeric(d)
            i <- i + 1
            r[ind] <- Rfast::eachcol.apply(x, res, indices = ind[ind > 
                0], oper = "*", apply = "sum")
            sel <- which.max(abs(r))
            sela <- c(sela, sel)
            options(warn = -1)
            mod <- Rfast::glm_logistic(x[, sela], y)
            est <- as.vector(exp(-mod$be[1] - x[, sela] %*% mod$be[-1]))
            res <- y - 1/(1 + est)
            rho[i] <- mod$devi
            ind[sela] <- 0
        }
    }
    else if (type == "poisson") {
        m <- sum(y)/n
        rho <- 2 * sum(y * log(y), na.rm = TRUE) - 2 * n * m * 
            log(m)
        ela <- as.vector( cov(y, x) )
        sel <- which.max( abs(ela) )
        sela <- sel
        names(sela) <- NULL
        options(warn = -1)
        mod <- Rfast::glm_poisson(x[, sel], y)
        res <- y - exp(mod$be[1] + x[, sel] * mod$be[2])
        rho[2] <- mod$devi
        ind[sel] <- 0
        i <- 2
        while ( (rho[i - 1] - rho[i]) > tol ) {
            r <- numeric(d)
            i <- i + 1
            r[ind] <- Rfast::eachcol.apply(x, res, indices = ind[ind > 
                0], oper = "*", apply = "sum")
            sel <- which.max(abs(r))
            sela <- c(sela, sel)
            options(warn = -1)
            mod <- Rfast::glm_poisson(x[, sela], y)
            res <- y - as.vector(exp(mod$be[1] + x[, sela] %*% 
                mod$be[-1]))
            rho[i] <- mod$devi
            ind[sela] <- 0
        }
    }
    else if (type == "quasipoisson") {
        m <- sum(y)/n
        rho <- 2 * sum(y * log(y), na.rm = TRUE) - 2 * n * m * 
            log(m)
        phi <- 1
        ela <- as.vector( cov(y, x) )
        sel <- which.max( abs(ela) )
        sela <- sel
        names(sela) <- NULL
        options(warn = -1)
        mod <- Rfast::qpois.reg(x[, sel], y)
        phi[2] <- mod$phi
        res <- y - exp(mod$be[1] + x[, sel] * mod$be[2])
        rho[2] <- mod$devi
        ind[sel] <- 0
        i <- 2
        while ( (rho[i - 1] - rho[i])/phi[i] > tol ) {
            r <- numeric(d)
            i <- i + 1
            r[ind] <- Rfast::eachcol.apply(x, res, indices = ind[ind > 
                0], oper = "*", apply = "sum")
            sel <- which.max(abs(r))
            sela <- c(sela, sel)
            options(warn = -1)
            mod <- Rfast::qpois.reg(x[, sela], y)
            res <- y - as.vector(exp(mod$be[1] + x[, sela] %*% 
                mod$be[-1]))
            rho[i] <- mod$devi
            phi[i] <- mod$phi
            ind[sela] <- 0
        }
    }
    else if (type == "quasibinomial") {
        p <- sum(y)/n
        y0 <- 1 - y
        rho <- 2 * sum(y * log(y/p), na.rm = TRUE) + 2 * sum(y0 * 
            log(y0/(1 - p)), na.rm = TRUE)
        phi[1] <- 1
        ela <- as.vector( cov(y, x) )
        sel <- which.max( abs(ela) )
        sela <- sel
        names(sela) <- NULL
        options(warn = -1)
        mod <- Rfast::prop.reg(y, x[, sel], varb = "glm")
        phi[2] <- mod$phi
        est <- exp(-mod$info[1, 1] - x[, sel] * mod$info[2, 1])
        p <- 1/(1 + est)
        res <- y - p
        rho[2] <- 2 * sum(y * log(y/p), na.rm = TRUE) + 2 * sum(y0 * 
            log(y0/(1 - p)), na.rm = TRUE)
        ind[sel] <- 0
        i <- 2
        while ( (rho[i - 1] - rho[i])/phi[i] > tol ) {
            r <- numeric(d)
            i <- i + 1
            r[ind] <- Rfast::eachcol.apply(x, res, indices = ind[ind > 
                0], oper = "*", apply = "sum")
            sel <- which.max(abs(r))
            sela <- c(sela, sel)
            options(warn = -1)
            mod <- Rfast::prop.reg(y, x[, sela], varb = "glm")
            est <- as.vector(exp(-mod$info[1, 1] - x[, sela] %*% 
                mod$info[-1, 1]))
            p <- 1/(1 + est)
            res <- y - p
            rho[i] <- 2 * sum(y * log(y/p), na.rm = TRUE) + 2 * 
                sum(y0 * log(y0/(1 - p)), na.rm = TRUE)
            phi[i] <- mod$phi
            ind[sela] <- 0
        }
    }
    else if (type == "normlog") {
        ini <- Rfast::normlog.mle(y)
        rho <- sum( (y - ini$param[1])^2 )
        phi[1] <- 1
        ela <- as.vector( cov(y, x) )
        sel <- which.max( abs(ela) )
        sela <- sel
        names(sela) <- NULL
        options(warn = -1)
        mod <- Rfast::normlog.reg(y, x[, sel])
        res <- y - exp(mod$be[1] + x[, sel] * mod$be[2])
        rho[2] <- mod$deviance
        phi[2] <- mod$devi/(n - 2)
        ind[sel] <- 0
        i <- 2
        while ( (rho[i - 1] - rho[i])/phi[i] > tol ) {
            r <- numeric(d)
            i <- i + 1
            r[ind] <- Rfast::eachcol.apply(x, res, indices = ind[ind > 
                0], oper = "*", apply = "sum")
            sel <- which.max(abs(r))
            sela <- c(sela, sel)
            options(warn = -1)
            mod <- Rfast::normlog.reg(y, x[, sela])
            res <- y - as.vector(exp(mod$be[1] + x[, sela] %*% 
                mod$be[-1]))
            rho[i] <- mod$deviance
            phi[i] <- mod$deviance/(n - length(mod$be))
            ind[sela] <- 0
        }
    }
    else if (type == "gamma") {
        ini <- Rfast::gammacon(y)
        rho <- ini$deviance
        phi[1] <- 1
        ela <- as.vector( cov(y, x) )
        sel <- which.max( abs(ela) )
        sela <- sel
        names(sela) <- NULL
        options(warn = -1)
        mod <- Rfast::gammareg(y, x[, sel])
        res <- y - exp(mod$be[1] + x[, sel] * mod$be[2])
        rho[2] <- mod$info[2]
        phi[2] <- mod$info[3]
        ind[sel] <- 0
        i <- 2
        while ( (rho[i - 1] - rho[i])/phi[i] > tol ) {
            r <- numeric(d)
            i <- i + 1
            r[ind] <- Rfast::eachcol.apply(x, res, indices = ind[ind > 
                0], oper = "*", apply = "sum")
            sel <- which.max(abs(r))
            sela <- c(sela, sel)
            options(warn = -1)
            mod <- Rfast::normlog.reg(y, x[, sela])
            res <- y - as.vector(exp(mod$be[1] + x[, sela] %*% 
                mod$be[-1]))
            rho[i] <- mod$info[2]
            phi[i] <- mod$info[3]
            ind[sela] <- 0
        }
    }
    else if (type == "weibull") {
        ini <- Rfast::weibull.mle(y)
        m <- ini$param[2]
        rho <- 2 * ini$loglik
        ela <- as.vector(cov(y - m, x))
        sel <- which.max(abs(ela))
        sela <- sel
        names(sela) <- NULL
        options(warn = -1)
        mod <- Rfast::weib.reg(y, x[, sel])
        res <- y - exp(mod$be[1] + x[, sel] * mod$be[2])
        rho[2] <- 2 * mod$loglik
        ind[sel] <- 0
        i <- 2
        while ( (rho[i] - rho[i - 1]) > tol ) {
            r <- numeric(d)
            i <- i + 1
            r[ind] <- Rfast::eachcol.apply(x, res, indices = ind[ind > 
                0], oper = "*", apply = "sum")
            sel <- which.max(abs(r))
            sela <- c(sela, sel)
            options(warn = -1)
            mod <- Rfast::weib.reg(y, x[, sela])
            res <- y - as.vector(exp(mod$be[1] + x[, sela] %*% 
                mod$be[-1]))
            rho[i] <- 2 * mod$loglik
            ind[sela] <- 0
        }
    }
    else if (type == "mv") {
        p <- dim(y)[2]
        con <- n * p * log(2 * pi) + n * p
        mod <- Rfast::mvnorm.mle(y)
        rho <- -2 * mod$loglik
        res <- Rfast::eachrow(y, mod$mu, oper = "-")
        ela <- numeric(d)
        ela <- Rfast::eachcol.apply(x, res[, 1], indices = ind, 
            oper = "*", apply = "sum")^2
        for (i in 2:p) ela <- ela + Rfast::eachcol.apply(x, res[, 
            i], indices = ind, oper = "*", apply = "sum")^2
        sel <- which.max(ela)
        sela <- sel
        names(sela) <- NULL
        res <- .lm.fit(cbind(1, x[, sela]), y)$residuals
        rho[2] <- con + n * log(det(crossprod(res)/(n - 1)))
        ind[sel] <- 0
        i <- 2
        while ( (rho[i - 1] - rho[i]) > tol ) {
            r <- numeric(d)
            i <- i + 1
            r[ind] <- Rfast::eachcol.apply(x, res[, 1], indices = ind[ind > 
                0], oper = "*", apply = "sum")^2
            for (j in 2:p) r[ind] <- r[ind] + Rfast::eachcol.apply(x, 
                res[, j], indices = ind[ind > 0], oper = "*", 
                apply = "sum")^2
            sel <- which.max(r)
            sela <- c(sela, sel)
            res <- .lm.fit(cbind(1, x[, sela]), y)$residuals
            rho[i] <- con + n * log(det(crossprod(res)/(n - i - 
                1)))
            ind[sela] <- 0
        }
    }
    else if (type == "multinomial") {
        p <- dim(y1)[2]
        for (j in 1:p) y1[, j] <- as.numeric(y1[, j])
        res <- Rfast::eachrow(y1, mod$prob[-1], oper = "-")
        ela <- numeric(d)
        ela <- Rfast::eachcol.apply(x, res[, 1], indices = ind, 
            oper = "*", apply = "sum")^2
        for (i in 2:p) ela <- ela + Rfast::eachcol.apply(x, res[, 
            i], indices = ind, oper = "*", apply = "sum")^2
        sel <- which.max(ela)
        sela <- sel
        names(sela) <- NULL
        mod <- try(Rfast::multinom.reg(y, x[, sela]), silent = TRUE)
        if (identical(class(mod), "try-error")) {
            rho[2] <- rho[1]
        }
        else {
            est <- exp(cbind(1, x[, sel]) %*% mod$be)
            est <- est/(Rfast::rowsums(est) + 1)
            res <- y1 - est
            rho[2] <- -2 * mod$loglik
            ind[sel] <- 0
        }
        i <- 2
        while ( (rho[i - 1] - rho[i]) > tol ) {
            r <- numeric(d)
            i <- i + 1
            r[ind] <- Rfast::eachcol.apply(x, res[, 1], indices = ind[ind > 
                0], oper = "*", apply = "sum")^2
            for (j in 2:p) r[ind] <- r[ind] + Rfast::eachcol.apply(x, 
                res[, j], indices = ind[ind > 0], oper = "*", 
                apply = "sum")^2
            sel <- which.max(r)
            sela <- c(sela, sel)
            mod <- try(Rfast::multinom.reg(y, x[, sela]), silent = TRUE)
            if (identical(class(mod), "try-error")) {
                rho[i] <- rho[i - 1]
            }
            else {
                rho[i] <- -2 * mod$loglik
                ind[sela] <- 0
                est <- exp(cbind(1, x[, sela]) %*% mod$be)
                est <- est/(Rfast::rowsums(est) + 1)
                res <- y1 - est
            }
        }
    }
    runtime <- proc.time() - tic
    len <- length(sela)
    info <- cbind(c(0, sela[-len]), rho[1:len])
    colnames(info) <- c("Selected Vars", "Deviance")
    if (!is.null(phi)) 
        phi <- phi[1:len]
    list(runtime = runtime, phi = phi, info = info)
}
