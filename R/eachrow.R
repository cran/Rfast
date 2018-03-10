
eachrow <- function(x,y,oper = "*",method = NULL){
	.Call('Rfast_eachrow', PACKAGE = 'Rfast',x,y,oper,method)
}