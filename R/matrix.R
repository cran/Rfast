#[export]
columns <- function(x,indices) {
	.Call(Rfast_columns,x,indices)
}

#[export]
rows <- function(x,indices) {
	.Call(Rfast_rows,x,indices)
}

#[export]
XopY.sum<-function(x,y=NULL,oper="*"){
	if(is.null(y)){
		.Call(Rfast_sum_XopX,x,oper)
	}
	else{
		.Call(Rfast_sum_XopY,x,y,oper)
	}
}

#[export]
submatrix <- function(x,rowStart=1,rowEnd=1,colStart=1,colEnd=1) {
  .Call(Rfast_submatrix,x,rowStart,rowEnd,colStart,colEnd)
}

#[export]
transpose <- function(x) {
	dm<-dim(x)
	if(dm[1]==dm[2]){
  		.Call(Rfast_transpose_sq,x)
  	}else{
  		.Call(Rfast_transpose_g_p,x)
  	}
}

#[export]
mat.mult <- function(x,y) {
	.Call(Rfast_mat_mult_p,t(x),y)
}

#[export]
mat.mat <- function(x, y) {
	.Call(Rfast_mat_mat,x,y)
}