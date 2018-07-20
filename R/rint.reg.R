rint.reg <- function (y, x, id, tol = 1e-08, ranef = FALSE, maxiters = 100) {
    x <- cbind(1, x)
    mod <- .Call(Rfast_rint_reg, x, y, id, tol, ranef, maxiters)
    names(mod$info) <- c("iters", "sigma_tau", "sigma_errors", "log-lik", 
        "deviance", "BIC")
    mod
}




# rint.reg <- function (y, x, id, tol = 1e-08) {
#     X <- cbind(1, x)
#     dm <- dim(X)
#     n <- dm[1]
#     p <- dm[2]
#     xx <- crossprod(X)
#     sx <- rowsum(X, id)
#     sxy <- crossprod(X, y)
#     sy <- as.vector( rowsum(y, id) )
#     ni <- tabulate(id)
#     mx <- sx/ni
#     my <- sy/ni
#     funa <- function(d, n, ni, S, hi2) sum(log1p(ni * d)) + n * 
#         log(S - d * sum(ni^2 * hi2/(1 + ni * d)))
#     mod <- .lm.fit(X, y)
#     b1 <- mod$coefficients
#     S <- sum(mod$residuals^2)
#     hi2 <- (my - mx %*% b1)^2
#     mod <- optimise(funa, c(0, 50), n = n, ni = ni, S = S, hi2 = hi2, 
#         tol = tol)
#     d <- mod$minimum
#     b2 <- solve(xx - d * crossprod(sx/(1 + ni * d), sx), sxy - 
#         d * crossprod(sx, sy/(1 + ni * d)))
#     i <- 2
#     while (sum(abs(b2 - b1)) > tol) {
#         i <- i + 1
#         b1 <- b2
#         S <- sum((y - X %*% b1)^2)
#         hi2 <- (my - mx %*% b1)^2
#         mod <- optimise(funa, c(0, 50), n = n, ni = ni, S = S, 
#             hi2 = hi2, tol = tol)
#         d <- mod$minimum
#         b2 <- solve(xx - d * crossprod(sx/(1 + ni * d), sx), 
#             sxy - d * crossprod(sx, sy/(1 + ni * d)))
#     }
#     se <- (S - d * sum(ni^2 * hi2/(1 + ni * d)))/n
#     tau <- d * se
#     loglik <- -0.5 * mod$objective - 0.5 * n * (log(2 * pi) - 
#         log(n) + 1)
#     dev <- -2 * loglik
#     seb <- sqrt(diag(solve(xx - d * crossprod(sx/(1 + ni * d), 
#         sx)) * se))
#     info <- c(i, tau, se, loglik, dev, dev + (p + 2) * log(n))
#     names(info) <- c("iters", "sigma_tau", "sigma_errors", "log-lik", 
#         "deviance", "BIC")
#     list(info = info, be = b2, seb = seb)
# }