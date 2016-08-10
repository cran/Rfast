dista <- function (xnew, x, type = "euclidean") {

    n <- nrow(x)
    xnew <- as.matrix(xnew)

    if ( ncol(xnew) > 1 ) { 
      nu <- nrow(xnew)
      disa <- matrix(0, nu, n)

      if (type == "euclidean") {
        y <- t(x)
        for (i in 1:nu) {
            z <- y - xnew[i, ]
            disa[i, ] <- sqrt( as.vector( colsums(z^2) ) )
        }

      } else if (type == "manhattan") {
        y <- t(x)
        for (i in 1:nu) {
            a <- y - xnew[i, ]
            disa[i, ] <- as.vector( colsums( abs(a) ) )
        }
      }
   
    } else {
      
      xnew <- as.vector(xnew)
      if ( type == "euclidean" ) {
         z <- t(x) - xnew
         disa <- sqrt( as.vector( rowsums(z^2) ) )

      } else if ( type == "manhattan" ) {
          z <- t(x) - xnew
          disa <- as.vector( rowsums( abs(a) ) )
      }
    
    }

    disa
}