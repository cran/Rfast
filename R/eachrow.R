
eachrow<-function(x,y,suma = FALSE,oper = "*"){
	if(suma){
		.Call('Rfast_sum_eachrow',x,y,oper)
	}else{
		.Call('Rfast_eachrow',x,y,oper)
	}
}