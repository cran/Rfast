#[export]
score.glms <- function(y, x, oiko = NULL, logged = FALSE ) {

 if ( is.null(oiko) ) {
   y <- as.numeric(y)
   if ( Rfast::sort_unique.length(y) == 2 ) {
       oiko <- "binomial"
   }   else oiko <- "poisson"
 } 

  n <- length(y) 
  r <- as.numeric( cor(y, x) )
  if ( oiko == "binomial" ) {
    stat <- r * sqrt(n)  
  } else  stat <- ( Var(y, std = T) / sqrt( sum(y) / n ) * sqrt(n - 1) ) * r 

  if ( logged ) {
    pvalue <- log(2) + pt( abs(stat), n - 2, lower.tail = FALSE, log.p = TRUE )
  } else  pvalue <- 2 * pt( abs(stat), n - 2, lower.tail = FALSE )
        
  cbind(stat, pvalue)
}
 
