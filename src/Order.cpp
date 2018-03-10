//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>
//[[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

using std::sort;
using std::stable_sort;


IntegerVector Order(NumericVector x,const bool stable,const bool descend){
	IntegerVector ind=seq(1,x.size());
	if(descend){
		auto descend_func = [&](int i,int j){return x[i-1]>x[j-1];};
		stable ? stable_sort(ind.begin(),ind.end(),descend_func) : sort(ind.begin(),ind.end(),descend_func);
	}else{
		auto func = [&](int i,int j){return x[i-1]<x[j-1];};
		stable ? stable_sort(ind.begin(),ind.end(),func) : sort(ind.begin(),ind.end(),func);
	}
	return ind;
}

//[[Rcpp::export]]
List Order2(NumericVector x,CharacterVector Nm){
  IntegerVector ind=seq(1,x.size());
  CharacterVector nm=clone(Nm);
  auto descend_func = [&](int i,int j){ if(x[i-1]<x[j-1]) {auto tmp=nm[i-1];nm[i-1]=nm[j-1];nm[j-1]=tmp; return true; }else return false;};
  sort(ind.begin(),ind.end(),descend_func);
  List L;
  L["names"]=nm;
  L["indices"]=ind;
  return L;
}

RcppExport SEXP Rfast_Order(SEXP xSEXP,SEXP stableSEXP,SEXP descendSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    __result = wrap(Order(x,stable,descend));
    return __result;
END_RCPP
}
