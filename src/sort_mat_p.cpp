//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>
#include "mn.h"

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
NumericMatrix sort_col_p(NumericMatrix& x,const bool descend){
  const int n=x.nrow(),p=x.ncol();
  NumericMatrix f(n,p);
  mat xx(x.begin(),n,p,false),ff(f.begin(),n,p,false);
  if(descend){
  	auto func=std::greater<double>();
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      colvec coli=xx.col(i);
      sort(coli.begin(),coli.end(),func);
      ff.col(i)=coli;
    }
  }else{
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      colvec coli=xx.col(i);
      sort(coli.begin(),coli.end());
      ff.col(i)=coli;
    }  
  }
  return f;
}

//[[Rcpp::export]]
NumericMatrix sort_row_p(NumericMatrix& x,const bool descend){
  const int n=x.nrow(),p=x.ncol();
  NumericMatrix f(n,p);
  mat xx(x.begin(),n,p,false),ff(f.begin(),n,p,false);
  if(descend){
  	auto func=std::greater<double>();
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<n;++i){
      rowvec rowi=xx.row(i);
      sort(rowi.begin(),rowi.end(),func);
      ff.row(i)=rowi;
    }
  }else{
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<n;++i){
      rowvec rowi=xx.row(i);
      sort(rowi.begin(),rowi.end());
      ff.row(i)=rowi;
    }  
  }
  return f;
}

//[[Rcpp::export]]
NumericMatrix sort_mat_p(NumericMatrix x,const bool descend,const bool by_row){
	return by_row ? sort_row_p(x,descend) : sort_col_p(x,descend);
}

// sort_mat_p
RcppExport SEXP Rfast_sort_mat_p(SEXP xSEXP,SEXP descendSEXP,SEXP by_rowSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type by_row(by_rowSEXP);
    __result = wrap(sort_mat_p(x,descend,by_row));
    return __result;
END_RCPP
}
