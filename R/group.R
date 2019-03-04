
group<-function(x,ina,method="sum",ina.min=NULL,ina.max = NULL,ina.length.unique=NULL,mad.method="median") {
	if(method=="med"){
		ina.max<- if(is.null(ina.length.unique)) length(unique(ina)) else ina.length.unique
	}
	.Call(Rfast_group,x,ina,method,ina.min,ina.max,mad.method)
}