//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>
#include "mn.h"

using namespace Rcpp;
using namespace std;

NumericMatrix sort_col(NumericMatrix x,const bool descend){
  const int n=x.nrow(),p=x.ncol();
  NumericVector coli(n);
  NumericMatrix f(n,p);
  if(descend)  
    for(int i=0;i<p;++i){
      coli=x.column(i);
      sort(coli.begin(),coli.end(),descending_double);
      f.column(i)=coli;
    }
  else
    for(int i=0;i<p;++i){
      coli=x.column(i);
      sort(coli.begin(),coli.end());
      f.column(i)=coli;
    }  
  return f;
}

NumericMatrix sort_row(NumericMatrix x,const bool descend){
  const int sz=x.ncol(),p=x.nrow();
  NumericVector rowi(sz);
  if(descend)
    for(int i=0;i<p;++i){
      rowi=x.row(i);
      sort(rowi.begin(),rowi.end(),descending_double);
      x.row(i)=rowi;
    }
  else
    for(int i=0;i<p;++i){
      rowi=x.row(i);
      sort(rowi.begin(),rowi.end());
      x.row(i)=rowi;
    }
  return x;
}

//[[Rcpp::export]]
NumericMatrix sort_mat(NumericMatrix x,const bool descend,const bool by_row){
	switch(by_row){
		case true:{
			return sort_row(x,descend);
		}
		default:{
			return sort_col(x,descend);
		}
	}
	stop("Error in sort matrix.\n");
}

// sort_mat
RcppExport SEXP Rfast_sort_mat(SEXP xSEXP,SEXP descendSEXP,SEXP by_rowSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type by_row(by_rowSEXP);
    __result = wrap(sort_mat(x,descend,by_row));
    return __result;
END_RCPP
}
