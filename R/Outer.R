
Outer<-function(x,y,oper="*"){
	if(identical(oper,"%%")) oper<-"%"
	.Call('Rfast_Outer',PACKAGE = "Rfast",x,y,oper)
}