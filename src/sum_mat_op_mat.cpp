// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "templates.h"

using namespace Rcpp;

double sum_XopY(SEXP x,SEXP y,const char oper){
  switch(oper){
    case '+': return sum_x_op_y< madd<double>,madd<double> >(x,y);
    case '-': return sum_x_op_y< mdiff<double>,madd<double> >(x,y);
    case '*': return sum_x_op_y< mmult<double>,madd<double> >(x,y);
    case '/': return sum_x_op_y< mdiv<double>,madd<double> >(x,y);
    default: stop("The operation doesn't supported.");
  }
  return 0.0;
}

RcppExport SEXP Rfast_sum_XopY(SEXP x,SEXP y,SEXP operSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const char  >::type oper(operSEXP);
    __result = wrap(sum_XopY(x,y,oper));
    return __result;
END_RCPP
}

////////////////////////////////////////////////////////////////////////


double sum_XopX(SEXP x,const char oper){
  switch(oper){
    case '+': return sum_x_op_x< madd<double>,madd<double> >(x);
    case '-': return sum_x_op_x< mdiff<double>,madd<double> >(x);
    case '*': return sum_x_op_x< mmult<double>,madd<double> >(x);
    case '/': return sum_x_op_x< mdiv<double>,madd<double> >(x);
    default: stop("The operation doesn't supported.");
  }
  return 0.0;
}

RcppExport SEXP Rfast_sum_XopX(SEXP x,SEXP operSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const char  >::type oper(operSEXP);
    __result = wrap(sum_XopX(x,oper));
    return __result;
END_RCPP
}

//////////////////////////////////////////////////////////////////////////
