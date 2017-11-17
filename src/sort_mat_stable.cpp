//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>
#include "mn.h"

using namespace Rcpp;
using namespace std;

NumericMatrix stable_sort_col(NumericMatrix x,const bool descend){
  const int n=x.nrow(),p=x.ncol();
  NumericVector coli(n);
  NumericMatrix f(n,p);
  if(descend){
  	auto func=std::greater<double>();
    for(int i=0;i<p;++i){
      coli=x.column(i);
      stable_sort(coli.begin(),coli.end(),func);
      f.column(i)=coli;
    }
}
  else
    for(int i=0;i<p;++i){
      coli=x.column(i);
      stable_sort(coli.begin(),coli.end());
      f.column(i)=coli;
    }
  return f;
}


//[[Rcpp::export]]
NumericMatrix stable_sort_row(NumericMatrix x,const bool descend){
  const int sz=x.ncol(),p=x.nrow();
  NumericVector rowi(sz);
  if(descend){
  	auto func=std::greater<double>();
    for(int i=0;i<p;++i){
      rowi=x.row(i);
      stable_sort(rowi.begin(),rowi.end(),func);
      x.row(i)=rowi;
    }
  }
  else
    for(int i=0;i<p;++i){
      rowi=x.row(i);
      stable_sort(rowi.begin(),rowi.end());
      x.row(i)=rowi;
    } 
  return x;
}

//[[Rcpp::export]]
NumericMatrix stable_sort_mat(NumericMatrix x,const bool descend,const bool by_row){
	return by_row ? stable_sort_row(x,descend) : stable_sort_col(x,descend);
}

// sort_mat
RcppExport SEXP Rfast_stable_sort_mat(SEXP xSEXP,SEXP descendSEXP,SEXP by_rowSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type by_row(by_rowSEXP);
    __result = wrap(stable_sort_mat(x,descend,by_row));
    return __result;
END_RCPP
}
