//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <Rinternals.h>
#include "templates.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
double med(SEXP x,const bool na_rm){
	double s=0;
	switch(TYPEOF(x)){
	  case REALSXP:{
    	  NumericVector xx(Rf_duplicate(x));
      	s= na_rm ? med_helper<NumericVector>(xx.begin(),xx.begin()+(std::remove_if(xx.begin(),xx.end(),R_IsNA)-xx.begin())) :
      	  med_helper<NumericVector>(xx.begin(),xx.end());
      	break;
	  }
	  case INTSXP:{
      	IntegerVector xx(Rf_duplicate(x));
	      s= na_rm ? med_helper<IntegerVector>(xx.begin(),xx.begin()+(std::remove_if(xx.begin(),xx.end(),R_IsNA)-xx.begin())) :
	        med_helper<IntegerVector>(xx.begin(),xx.end());
      	break;
	  }
    default:
      stop("Error: Unknown type.\n");
      break;
  }
  return s;
}

//returns median of a vector
RcppExport SEXP Rfast_med(SEXP x,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = wrap(med(x,na_rm));
    return __result;
END_RCPP
}
