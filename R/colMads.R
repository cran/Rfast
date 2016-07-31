colMads = function(x, ncores = 2) {
  
  if ( ncores == 1 ) {
    m = colMedians(x)
    y = t( t(x) - m )
    colMedians( abs(y) ) / qnorm(3/4)
  } else {
    
    m = colMedians_parallel(x, ncores)
    y = t( t(x) - m )
    colMedians_parallel( abs(y), ncores ) / qnorm(3/4)
  }

}