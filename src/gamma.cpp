//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <Rinternals.h>
#include <R.h>
#include "mn.h"

using namespace Rcpp;
using std::tgamma;

SEXP Trigamma(SEXP x){
  int n=LENGTH(x);
  SEXP f=PROTECT(Rf_duplicate(x));
  switch(TYPEOF(x)){
    case REALSXP:{
      double *start_f=REAL(f),*start_x=REAL(x),*end_x=start_x+n;
      for(;start_x!=end_x;++start_x,++start_f)
        *start_f=trigamma(*start_x);
      break;
    }
    default:{
      int *start_f=INTEGER(f),*start_x=INTEGER(x),*end_x=start_x+n;
      for(;start_x!=end_x;++start_x,++start_f)
        *start_f=trigamma(*start_x);
      break;
    }
  }
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_Trigamma(SEXP x){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = Trigamma(x);
    return __result;
END_RCPP
}


SEXP Digamma(SEXP x){
  int n=LENGTH(x);
  SEXP f=PROTECT(Rf_duplicate(x));
  switch(TYPEOF(x)){
    case REALSXP:{
      double *start_f=REAL(f),*start_x=REAL(x),*end_x=start_x+n;
      for(;start_x!=end_x;++start_x,++start_f)
        *start_f=digamma(*start_x);
      break;
    }
    default:{
      int *start_f=INTEGER(f),*start_x=INTEGER(x),*end_x=start_x+n;
      for(;start_x!=end_x;++start_x,++start_f)
        *start_f=digamma(*start_x);
      break;
    }
  }
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_Digamma(SEXP x){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = Digamma(x);
    return __result;
END_RCPP
}

SEXP Lgamma(SEXP x){
  int n=LENGTH(x);
  SEXP f=PROTECT(Rf_duplicate(x));
  switch(TYPEOF(x)){
    case REALSXP:{
      double *start_f=REAL(f),*start_x=REAL(x),*end_x=start_x+n;
      for(;start_x!=end_x;++start_x,++start_f)
        *start_f=lgamma(*start_x);
      break;
    }
    default:{
      int *start_f=INTEGER(f),*start_x=INTEGER(x),*end_x=start_x+n;
      for(;start_x!=end_x;++start_x,++start_f)
        *start_f=lgamma(*start_x);
      break;
    }
  }
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_Lgamma(SEXP x){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = Lgamma(x);
    return __result;
END_RCPP
}

//////////////////////////////////////////////////////////////////////////////

SEXP Choose(SEXP n,const int k){
  const int tgk_1=tgamma(k+1);
  int len=LENGTH(n);
  SEXP f=PROTECT(Rf_allocVector(REALSXP,len));
  double *start_f=REAL(f);
  switch(TYPEOF(n)){
    case INTSXP:{
      int *start=INTEGER(n),*end=start+len;
      for(;start!=end;++start,++start_f)
        *start_f=tgamma(*start+1)/(tgk_1*tgamma(*start-k+1));
      break;
    }
    default:{
      double *start=REAL(n),*end=start+len;
      for(;start!=end;++start,++start_f)
        *start_f=tgamma(*start+1)/(tgk_1*tgamma(*start-k+1));
      break;
    }
  }
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_Choose(SEXP x,SEXP kSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const int >::type k(kSEXP);
    __result = Choose(x,k);
    return __result;
END_RCPP
}

//////////////////////////////////////////////////////////////////////////////


using std::lgamma;

SEXP Lchoose(SEXP n,const int k){
  const double lgk_1=lgamma(k+1);
  int len=LENGTH(n);
  SEXP f=PROTECT(Rf_allocVector(REALSXP,len));
  double *start_f=REAL(f);
  switch(TYPEOF(n)){
  case INTSXP:{
    int *start=INTEGER(n),*end=start+len;
    for(;start!=end;++start,++start_f)
      *start_f=lgamma(*start+1)-lgk_1-lgamma(*start-k+1);
    break;
  }
  default:{
    double *start=REAL(n),*end=start+len;
    for(;start!=end;++start,++start_f)
      *start_f=lgamma(*start+1)-lgk_1-lgamma(*start-k+1);
    break;
  }
  }
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_Lchoose(SEXP x,SEXP kSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const int >::type k(kSEXP);
    __result = Lchoose(x,k);
    return __result;
END_RCPP
}

//////////////////////////////////////////////////////////////////////////

using namespace Rcpp;
using std::prev_permutation;
using std::vector;

SEXP k_comb_n(const int N, const int K){
  SEXP x=Rf_allocVector(INTSXP,K*R::choose(N,K));
  int *xx=INTEGER(x);
  string bitmask(K, 1); // K leading 1's
  bitmask.resize(N, 0); // N-K trailing 0's
  int i;
  do {
    for(i=0;i<N;++i) // [0..N-1] integers
      if(bitmask[i]) 
        *xx++=i+1;
  }while(prev_permutation(bitmask.begin(),bitmask.end()));
  return x;
}

RcppExport SEXP Rfast_k_comb_n(SEXP nSEXP,SEXP kSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const int >::type n(nSEXP);
    traits::input_parameter< const int >::type k(kSEXP);
    __result = k_comb_n(n,k);
    return __result;
END_RCPP
}

/////////////////////////////////////////////////////////////////////////////////////

NumericMatrix find_combn(NumericVector data, const int n) {
	static int combn_col = 0;
	const int size = data.size();
	const int nrows = n;
	const int ncols = R::choose(size, n); 
	NumericMatrix combn_dataset(nrows, ncols);
	std::vector<double> combn_data(n);
	const int start_idx = 0;
	combn_col = 0; 
	combn(data, n, start_idx, combn_data, combn_dataset, combn_col);
	return combn_dataset;
}

RcppExport SEXP Rfast_vec_comb_n(SEXP dataSEXP,SEXP nSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type data(dataSEXP);
    traits::input_parameter< const int >::type n(nSEXP);
    __result = wrap(find_combn(data,n));
    return __result;
END_RCPP
}
