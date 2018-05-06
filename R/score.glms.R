score.glms <- function(y, x, oiko = NULL, logged = FALSE ) {

 if ( is.null(oiko) ) {
   if ( sort_unique.length(y) == 2 ) {
       oiko = "binomial"
   }   else oiko = "poisson"
 } 

  n <- length(y) 
  sx2 <- Rfast::colsums(x^2)
  my <- sum(y) / n
  sx <- Rfast::colsums(x)
  up <- as.vector( cov(y, x) ) * (n - 1)  ## score function
  if ( oiko == "binomial" ) {
    down <- ( sx2 - sx^2 / n ) * ( my - my^2 ) ## variance
  } else   down <- ( sx2 - sx^2 / n ) * my ## variance
  stat <- up / sqrt(down)
  if ( logged ) {
    pvalue <- log(2) + pt( abs(stat), n - 2, lower.tail = FALSE, log.p = TRUE )
  } else  pvalue <- 2 * pt( abs(stat), n - 2, lower.tail = FALSE )
        
  cbind(stat, pvalue)
}
 