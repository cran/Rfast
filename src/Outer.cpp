#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>

#include "templates.h"

using namespace Rcpp;

using std::fmod;
using std::pow;

SEXP Outer(SEXP x,SEXP y,const char oper='*'){
  int lenx=LENGTH(x),leny=LENGTH(y);
  SEXP F=PROTECT(Rf_allocMatrix(REALSXP,leny,lenx));
  double *xx=REAL(x),*end=xx+lenx,*f=REAL(F),*yy=REAL(y);
  
  switch(oper){
  case '*': 
    for(;xx!=end;++xx,f+=leny)
      myoperator<double,mmult<double> >(f,*xx,yy,leny);
    break;
  case '-':
    for(;xx!=end;++xx,f+=leny)
      myoperator<double,mdiff<double> >(f,*xx,yy,leny);
    break;
  case '+':
    for(;xx!=end;++xx,f+=leny)
      myoperator<double,madd<double> >(f,*xx,yy,leny);
    break;
  case '/':
    for(;xx!=end;++xx,f+=leny)
      myoperator<double,mdiv<double> >(f,*xx,yy,leny);
    break;
  case '^':
    for(;xx!=end;++xx,f+=leny)
      myoperator<double,std::pow>(f,*xx,yy,leny);
    break;
  case '%':
    for(;xx!=end;++xx,f+=leny)
      myoperator<double,std::fmod>(f,*xx,yy,leny);
    break;
  default:
    stop("Wrong operator.\n");
  }
  UNPROTECT(1);
  return F;
}



RcppExport SEXP Rfast_Outer(SEXP x,SEXP y,SEXP operSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const char >::type oper(operSEXP);
    __result = Outer(x,y,oper);
    return __result;
END_RCPP
}
