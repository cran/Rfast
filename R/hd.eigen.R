hd.eigen <- function(x, center = TRUE, scale = FALSE, k = NULL, vectors = FALSE) {
    n <- dim(x)[1]
    if (center & scale) {
        y <- t(x) - colmeans(x)
        y <- y/sqrt(rowsums(y^2)) * sqrt(n - 1)
        xx <- crossprod(y)
    }   else if (center & !scale) {
        m <- colmeans(x) 
        y <- t(x) - m
        xx <- crossprod(y)
    }   else if (!center & scale) {
        s <- colVars(x, std = TRUE)
        y <- t(x) / s 
        xx <- crossprod(y)
    }   else {
        y <- t(x)
        xx <- crossprod(y)
    }	
    a <- eigen(xx )
    if ( is.null(k) )   k <- n
    L <- a$values[1:k]
	
    if (vectors) {
	FF <- submatrix(a$vectors, 1, n, 1, k)
      vectors <- tcrossprod(y, t(FF) * L^(-0.5) )
    } else  vectors <- NULL
    list(values = L/(n - 1), vectors = vectors)
}

