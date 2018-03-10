
eachcol.apply<-function(x,y,indices = NULL,oper = "*",apply = "sum"){
	.Call('Rfast_eachcol_apply',PACKAGE = "Rfast",x,y,indices,oper,apply)
}