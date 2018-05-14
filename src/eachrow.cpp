
//Author: Manos Papadakis
//[[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include "templates.h"

using namespace Rcpp;
using namespace std;


static SEXP eachrow(SEXP x,SEXP y,const char oper){
  switch(oper){
    case '*': return eachrow_helper< mmult<double> >(x,y);
    case '+': return eachrow_helper< madd<double> >(x,y);
    case '/': return eachrow_helper< mdiv<double> >(x,y);
    case '-': return eachrow_helper< mdiff<double> >(x,y);
    case '^': return eachrow_helper<std::pow >(x,y);
    default: stop("The operation doesn't supported.");
  }
  return R_NilValue;
}

static double apply_eachrow(SEXP x,SEXP y,const char oper,const string method){
  if(method == "sum"){
    switch(oper){
      case '*': return apply_eachrow_helper<mmult<double>,madd<double> >(x,y);
      case '+': return apply_eachrow_helper<madd<double>,madd<double> >(x,y);
      case '/': return apply_eachrow_helper<mdiv<double>,madd<double> >(x,y);
      case '-': return apply_eachrow_helper<mdiff<double>,madd<double> >(x,y);
      case '^': return apply_eachrow_helper<std::pow,madd<double> >(x,y);
      default: stop("The operation doesn't supported.");
    }
  }else if(method == "min"){
    switch(oper){
      case '*': return apply_eachrow_helper<mmult<double>,mmin<double> >(x,y);
      case '+': return apply_eachrow_helper<madd<double>,mmin<double> >(x,y);
      case '/': return apply_eachrow_helper<mdiv<double>,mmin<double> >(x,y);
      case '-': return apply_eachrow_helper<mdiff<double>,mmin<double> >(x,y);
      case '^': return apply_eachrow_helper<std::pow,mmin<double> >(x,y);
      default: stop("The operation doesn't supported.");
    }
  }else if(method == "max"){
    switch(oper){
      case '*': return apply_eachrow_helper<mmult<double>,mmax<double> >(x,y);
      case '+': return apply_eachrow_helper<madd<double>,mmax<double> >(x,y);
      case '/': return apply_eachrow_helper<mdiv<double>,mmax<double> >(x,y);
      case '-': return apply_eachrow_helper<mdiff<double>,mmax<double> >(x,y);
      case '^': return apply_eachrow_helper<std::pow,mmax<double> >(x,y);
      default: stop("The operation doesn't supported.");
    }
  }
  return 0.0;
}

SEXP eachrow(SEXP x,SEXP y,const char oper,SEXP meth){
  return Rf_isNull(meth) ? eachrow(x,y,oper) : wrap(apply_eachrow(x,y,oper,as<string>(meth)));
}

RcppExport SEXP Rfast_eachrow(SEXP x,SEXP y,SEXP operSEXP,SEXP method){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const char  >::type oper(operSEXP);
    __result = eachrow(x,y,oper,method);
    return __result;
END_RCPP
}
