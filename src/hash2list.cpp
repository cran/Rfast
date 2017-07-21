// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <algorithm>
#include "mn.h"

using namespace std;
using namespace Rcpp;

//[[Rcpp::export]]
List hash2list(List x,bool sorting){
	CharacterVector nam=x.names();
	vector<string> lnames(nam.begin(),nam.end());
	vector<string>::iterator names=lnames.begin();
	NumericVector val,rr;
	int sz=lnames.size();
	List f(sz);
	List::iterator ff=f.begin();
	if(sorting){
		for(;ff!=f.end();++ff,++names){
			rr=toNumbers(*names,' ');
			sort(rr.begin(),rr.end());
			val=x[*names];
			rr.push_back(val[0]);
			*ff=rr;
		}
	}else{
	  	for(;ff!=f.end();++ff,++names){
			rr=toNumbers(*names,' ');
			val=x[*names];
			rr.push_back(val[0]);
			*ff=rr;
	  	}
	}
  return f;
}

RcppExport SEXP Rfast_hash2list(SEXP xSEXP,SEXP sortingSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< List >::type x(xSEXP);
    traits::input_parameter< bool >::type sorting(sortingSEXP);
    __result = wrap(hash2list(x,sorting));
    return __result;
END_RCPP
}
