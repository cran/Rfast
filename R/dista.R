dista <- function (xnew, x, type = "euclidean") {
   n <- dim(x)[1]
   if ( is.matrix(xnew) ) { 
     nu <- dim(xnew)[1]
     disa <- matrix(0, nu, n)
     if (type == "euclidean") {
       y <- t(x)
       for (i in 1:nu)  disa[i, ] <- sqrt( colsums( (y - xnew[i, ]) ^2) )
     } else if (type == "manhattan") {
       y <- t(x)
       for (i in 1:nu)  disa[i, ] <- colsums( abs( y - xnew[i, ] ) )
     }
   
   } else {
     if ( type == "euclidean" ) {
       disa <- sqrt( rowsums( (t(x) - xnew) ^2) )
     } else if ( type == "manhattan" ) {
       disa <- rowsums( abs( z <- t(x) - xnew ) )
     }
   }
   
   disa
}