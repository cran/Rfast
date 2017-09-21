//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
NumericVector col_nth(NumericMatrix x,IntegerVector elems){
  const int n=elems.size();
  NumericVector f(n),y(x.nrow());
  NumericVector::iterator ff=f.begin();
  IntegerVector::iterator els=elems.begin();
  for(int i=0;i!=n;++ff,++i,++els){
    y=x.column(i);
    nth_element(y.begin(),y.begin() + *els-1 ,y.end());
    *ff=y[*els-1];
  }
  return f;
}

// nth_element
RcppExport SEXP Rfast_col_nth(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type y(ySEXP);
    __result = wrap(col_nth(x,y));
    return __result;
END_RCPP
}


//[[Rcpp::export]]
NumericVector row_nth(NumericMatrix x,IntegerVector elems){
  const int n=elems.size();
  NumericVector f(n),y(x.ncol());
  NumericVector::iterator ff=f.begin();
  IntegerVector::iterator els=elems.begin();
  for(int i=0;i!=n;++ff,++i,++els){
    y=x.row(i);
    nth_element(y.begin(),y.begin()+*els-1,y.end());
    *ff=y[*els-1];
  }
  return f;
}

RcppExport SEXP Rfast_row_nth(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type y(ySEXP);
    __result = wrap(row_nth(x,y));
    return __result;
END_RCPP
}
