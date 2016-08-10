colMads = function(x) {
  
    m = colMedians(x)
    y = t( t(x) - m )
    colMedians( abs(y) ) / qnorm(3/4)

}