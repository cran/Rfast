//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
double Norm(NumericMatrix x, const char type) {
	if(type=='F')
		return sumsqr(x);
	mat xx(x.begin(),x.nrow(),x.ncol(),false);
	switch(type){
		case 'C':{
			rowvec a=sum(abs(xx),0);
			return a[a.index_max()];
		}
		case 'R':{
			mat xx(x.begin(),x.nrow(),x.ncol(),false);
			colvec a=sum(abs(xx),1);
			return a[a.index_max()];
		}
		case 'M':{
			return xx[xx.index_max()];
			
		}
		default:{
			stop("Wrong type. You have to give one of <F,C,R,M>.\n");
		}
	}
	return 0.0;
}


RcppExport SEXP Rfast_Norm(SEXP xSEXP,SEXP typeSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const char  >::type type(typeSEXP);
    __result = wrap(Norm(x,type));
    return __result;
END_RCPP
}
