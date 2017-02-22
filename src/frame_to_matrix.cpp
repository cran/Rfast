//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <Rinternals.h>
#include "mn.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
NumericMatrix frame_to_matrix(DataFrame x,Function as_numeric){
  vector<int> num_of_fac=which_isFactor(x);
  int i=0,p=x.length(),n=x.nrows();
  NumericMatrix f(n,p);
  NumericVector a;
  if(!num_of_fac.size()){
    for(DataFrame::iterator xx=x.begin();xx!=x.end();++xx,++i){
      a=*xx;
      f(_,i)=a;
    }
    return f;
  }
  vector<int>::iterator iter_num_of_fac=num_of_fac.begin();
  for(DataFrame::iterator xx=x.begin();xx!=x.end();++xx,++i)
    if(i==*iter_num_of_fac){
      a=as_numeric(*xx);      
      f(_,i)=a;                       
      iter_num_of_fac++;
    }else{
      a=*xx;
      f(_,i)=a;
    }
    return f;
}

RcppExport SEXP Rfast_frame_to_matrix(SEXP xSEXP,SEXP as_numericSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< DataFrame >::type x(xSEXP);
    traits::input_parameter< Function >::type as_numeric(as_numericSEXP);
    __result = wrap(frame_to_matrix(x,as_numeric));
    return __result;
END_RCPP
}
