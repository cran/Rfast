univglms <- function(y, x, oiko = NULL, logged = FALSE) {
   
   ## y is the target variable
   ## x is a matrix or a data frame with predictor variables
   n <- length(y)  ## sample size
   x <- as.matrix(x)
   d <- ncol(x)
  

  if ( is.null(oiko) ) {

    if ( length( unique(y) ) == 2 ) {   
        
      oiko = "binomial"

    } else if ( length( unique(y) ) > 2  & sum( round(y) - y ) == 0 ) {
     
      oiko = "poisson"
  
    } else {
    
      oiko = "normal"

    }
  
  } 


   if ( oiko == "binomial" ) {

      y <- as.numeric( as.factor(y) ) - 1

       p <- sum(y) / n
       ini <-  - 2 * sum( y * log(p) + (1 - y) * log(1 - p) )  ## initial deviance
       mod <- logistic_only(x, y)  ## deviance of each predictor variable
       stat <- abs( mod - ini )

       if ( any( is.na(stat) ) ) {
         poio <- which( is.na(stat) )
         for (i in poio) {
           a <- glm( y ~ x[, poio], binomial )
           stat[i] <- abs( a$deviance - ini )
         }
       }

       if ( logged == TRUE ) {
         pval <- pchisq(stat, 1, lower.tail = FALSE, log.p = TRUE)
       } else {
         pval <- pchisq(stat, 1, lower.tail = FALSE)
       }
       
   } else if ( oiko == "poisson" ) {
       
       y <- as.vector(y)
       m <- sum(y) / n  
       ini <- 2 * ( sum(y * log(y / m), na.rm = TRUE ) ) 
       mod <- poisson_only(x, y)  ## deviance of each predictor variable
       stat <- abs( mod - ini )

       if ( any( is.na(stat) ) ) {
         poio <- which( is.na(stat) )
         for (i in poio) {
           a <- glm( y ~ x[, poio], poisson )
           stat[i] <- abs( a$deviance - ini ) 
         }
       }
	   
       if ( logged == TRUE ) {
         pval <- pchisq(stat, 1, lower.tail = FALSE, log.p = TRUE)
       } else {
         pval <- pchisq(stat, 1, lower.tail = FALSE)
       }

   } else if ( oiko == "normal" ) { 
     
      if ( min(y) > 0 & max(y) < 1) {
        y <- log( y / (1 - y) )
      } 
     
      rho <- as.vector( cor(y, x) )
      
      sqdof <- sqrt(n - 2)
      stat <- rho * sqdof / sqrt(1 - rho^2)
      stat <- stat^2
      
      if ( logged == TRUE ) {
        pval <- pf(stat, 1, n - 2, lower.tail = FALSE, log.p = TRUE)
      } else {
        pval <- pf(stat, 1, n - 2, lower.tail = FALSE)
      }
      
   }
   
     result <- cbind(stat, pval) 

     if ( is.null( colnames(x) ) ) {
       row.names(result) <- paste("Var", 1:d, sep = "")
     } else  row.names(result) <- colnames(x)
 
    result
}
     