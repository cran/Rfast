colrange <- function(x, cont = TRUE) {

   if ( cont == TRUE ) {
     evros <- colMaxs(x, TRUE) - colMins(x, TRUE)
   } else {
     evros <- colMaxs(x, TRUE) - colMins(x, TRUE) + 1
   }
  
  evros

}


