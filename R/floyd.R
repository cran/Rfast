
floyd<-function(x){
  i4_huge<-2147483647
  y<-as.vector(x)
  y[is.na(y)]<-i4_huge
  dm<-ncol(x)
  y<-.Call('Rfast_floyd_john', PACKAGE = 'Rfast',dm,y)
  y[y==i4_huge]<-NA
  dim(y)<-c(dm,dm)
  y
}