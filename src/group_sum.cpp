
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"
#include <algorithm>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
NumericVector group_sum(NumericVector x, IntegerVector f){
  std::vector<double> vec(10) ;
  vec.reserve(1000); 
  const int n=x.size();
  unsigned int index,vecsz=vec.size();
  for( int i=0; i<n; i++){
    index=f[i]; 
    while( index >= vecsz ){
      vec.resize( vecsz * 2 ) ; 
      vecsz=vec.size();
    }
    vec[ index ] += x[i] ;
  }
  // count the number of non zeros
  int s = std::accumulate( vec.begin(), vec.end(), 0, increment_maybe) ; 
  NumericVector result(s) ;
  CharacterVector names(s) ;
  
  std::vector<double>::iterator it = vec.begin() ;
  for( int i=0, j=0 ; j<s; j++ ,++it, ++i ){
    // move until the next non zero value
    while( ! *it ){ 
    	++i ; 
    	++it ;
    }
    result[j] = *it ;
    names[j]  = i ;
  }
  result.attr( "dim" ) = IntegerVector::create(s, 1) ;
  result.attr( "dimnames" ) = List::create(names, R_NilValue) ; 
  return result ;
}

RcppExport SEXP Rfast_group_sum(SEXP xSEXP,SEXP groupSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    __result = wrap(group_sum(x,group));
    return __result;
END_RCPP
}
