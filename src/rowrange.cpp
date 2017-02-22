//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <vector>

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
vector<double> rowrange(NumericMatrix x,bool cont){
  const unsigned int p=x.nrow();
  mat X = mat(x.begin(), p, x.ncol(), false); 
  colvec f=max(X,1)-min(X,1);
  colvec::iterator ff=f.begin();
  vector<double> F(p);
  vector<double>::iterator FF=F.begin();
  if(cont)
  	for(;FF!=F.end();++FF,++ff)
  	  *FF=*ff;
  else
  	for(;FF!=F.end();++FF,++ff)
	  *FF=*ff+1;
  return F;
}

RcppExport SEXP Rfast_rowrange(SEXP xSEXP,SEXP contSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< bool >::type cont(contSEXP);
    __result = wrap(rowrange(x,cont));
    return __result;
END_RCPP
}
