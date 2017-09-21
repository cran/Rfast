//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace arma;

//[[Rcpp::export]]
mat dista(NumericMatrix Xnew, NumericMatrix X,const bool sqr,const string type = "euclidean") {
  const int n=X.ncol(),nu=Xnew.ncol();
  mat xnew(Xnew.begin(),Xnew.nrow(),nu,false),x(X.begin(),X.nrow(),n,false);
  mat disa(n,nu,fill::zeros);
  if(type == "euclidean"){
    if(sqr){
      for(int i=0;i<nu;++i)
        disa.col(i)=sum(square(x.each_col() - xnew.col(i)),0);
    }else{
      for(int i=0;i<nu;++i)
        disa.col(i)=sqrt_mat(sum(square(x.each_col() - xnew.col(i)),0));
    }
  }else if(type == "manhattan"){
    for(int i=0;i<nu;++i)
      disa.col(i)=(sum(abs(x.each_col() - xnew.col(i)),0)).t();
  }
  else stop("Unknown type argument. you have to enter \"euclidean\" or \"manhattan\".");
  return disa;
}

RcppExport SEXP Rfast_dista(SEXP XnewSEXP,SEXP XSEXP,SEXP sqrSEXP,SEXP typeSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type Xnew(XnewSEXP);
    traits::input_parameter< NumericMatrix >::type X(XSEXP);
    traits::input_parameter< const bool >::type sqr(sqrSEXP);
    traits::input_parameter< const string >::type type(typeSEXP);
    __result = wrap(dista(Xnew,X,sqr,type));
    return __result;

END_RCPP
}
