#[export]
rowVars <- function(x, suma = NULL, std = FALSE) {
  if ( !is.null(suma) ) {
    m <- suma
  } else  m <- Rfast::rowsums(x)
  n <- dim(x)[2]
  x2 <- Rfast::rowsums(x^2)
  s <- ( x2 - m^2/n ) / (n - 1)
  if ( std )  s <- sqrt(s)
  s
}

#[export]
colVars <- function(x, suma = NULL, std = FALSE, parallel = FALSE) {
  ##  x is a matrix
  ##  if you want standard deviations set std = TRUE
  if ( !is.null(suma) ) {
    m <- suma
  } else  m <- Rfast::colsums(x, parallel = parallel)
  n <- dim(x)[1]
  x2 <- Rfast::colsums(x^2, parallel = parallel)
  s <- ( x2 - m^2/n ) / (n - 1)
  if ( std )  s <- sqrt(s)
  s
}

#[export]
Var <- function(x,std = FALSE,na.rm = FALSE) {
	if(na.rm){
		f <- .Call(Rfast_var_c_na_rm,x,which(!is.na(x)))
	}else{
		f <- .Call(Rfast_var_c,x)
	}
  	if(std) f <- sqrt(f)
  	f
}