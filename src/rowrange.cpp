//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <vector>
#include "mn.h"

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
vector<double> rowrange(SEXP y,bool cont){
	const unsigned int p=Rf_nrows(y);
	vector<double> F(p);
	vector<double>::iterator FF=F.begin();
	if(cont){
		NumericMatrix x(y);
		mat X = mat(x.begin(), p, x.ncol(), false); 
	  	colvec f=max(X,1)-min(X,1);
	  	colvec::iterator ff=f.begin();
	  	for(;FF!=F.end();++FF,++ff)
	  	  *FF=*ff;
	}else{
		IntegerMatrix x(y);
	  	for(int i=0;FF!=F.end();++FF,++i)
		  *FF=len_sort_unique_int(x.row(i));
	}
  return F;
}

RcppExport SEXP Rfast_rowrange(SEXP x,SEXP contSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< bool >::type cont(contSEXP);
    __result = wrap(rowrange(x,cont));
    return __result;
END_RCPP
}
