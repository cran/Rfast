univglms <- function(y, x, oiko = NULL, logged = FALSE) {
   
   ## y is the target variable
   ## x is a matrix or a data frame with predictor variables
   dm <- dim(x)
   n <- dm[1]  ## sample size
   x <- as.matrix(x)
   d <- dm[2] 

  if ( is.null(oiko) ) {
    
    if ( length( sort_unique(y) ) == 2 ) {   
        
      oiko = "binomial"

    } else if ( sum( round(y) - y ) == 0 ) {
     
      oiko = "poisson"
  
    } else  oiko = "normal"
  
  } 

   if ( oiko == "binomial" ) {

       p <- sum(y) / n
       ini <-  - 2 * sum( y * log(p) + (1 - y) * log(1 - p) )  ## initial deviance
       mod <- logistic_only(x, y)  ## deviance of each predictor variable
       stat <- ini - mod

       if ( any( is.na(stat) ) ) {
         poio <- which( is.na(stat) )
         for (i in poio) {
           a <- glm( y ~ x[, poio], binomial )$deviance
           stat[i] <- ini - a$deviance
         }
       }      
       pval <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
       
   } else if ( oiko == "poisson" ) {
       
       m <- sum(y) / n  
       ini <- 2 * sum(y * log(y / m), na.rm = TRUE ) 
       mod <- poisson_only(x, y)  ## deviance of each predictor variable
       stat <- ini - mod

       if ( any( is.na(stat) ) ) {
         poio <- which( is.na(stat) )
         for (i in poio) {
           a <- glm( y ~ x[, poio], poisson )$deviance
           stat[i] <- ini - a
         }
       }
       pval <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)

   } else if ( oiko == "normal" ) { 
      mn_mx <- min_max(y)
      if ( mn_mx[1] > 0  &  mn_mx[2] < 1) {
        y <- log( y / (1 - y) )
      } 
     
      rho <- as.vector( cor(y, x) )     
      sqdof <- sqrt(n - 2)
      stat <- rho * sqdof / sqrt(1 - rho^2)
      
      if ( logged ) {
        pval <- log(2) + pt( abs(stat), n - 2, lower.tail = FALSE, log.p = TRUE)
      } else  pval <- 2 * pt( abs(stat), n - 2, lower.tail = FALSE)
   }
   
    result <- cbind(stat, pval) 
    colnames(result) <- c("stat", "pvalue") 

    if ( is.null( colnames(x) ) ) {
      rownames(result) <- paste("Var", 1:d, sep = "")
    } else  row.names(result) <- colnames(x)
 
    result
}
     