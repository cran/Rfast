//Author: Manos Papadakis

#include <RcppArmadillo.h>

#ifdef _OPENMP
#include <omp.h>
#endif


using namespace Rcpp;
using namespace std;
using namespace arma;

icolvec cOrder(colvec x,const bool stable,const bool descend){
  icolvec ind=linspace<icolvec>(1,x.size(),x.size());
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
IntegerMatrix col_order_p(NumericMatrix x,const bool stable,const bool descending){
	const int ncl=x.ncol(),nrw=x.nrow();
	IntegerMatrix f(nrw,ncl);
	mat xx(x.begin(),nrw,ncl,false);
	imat ff(f.begin(),nrw,ncl,false);
	#ifdef _OPENMP
    #pragma omp parallel for
    #endif
  	for(int i=0;i<ncl;++i){
  		ff.col(i)=cOrder(xx.col(i),stable,descending);
  	}
  	return f;
}

RcppExport SEXP Rfast_col_order_p(SEXP xSEXP,SEXP stableSEXP,SEXP descendingSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    traits::input_parameter< const bool >::type descending(descendingSEXP);
    __result = wrap(col_order_p(x,stable,descending));
    return __result;
END_RCPP
}

irowvec rOrder(rowvec x,const bool stable,const bool descend){
  irowvec ind=linspace<irowvec>(1,x.size(),x.size());
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
IntegerMatrix row_order_p(NumericMatrix x,const bool stable,const bool descending){
	const int ncl=x.ncol(),nrw=x.nrow();
	IntegerMatrix f(nrw,ncl);
	mat xx(x.begin(),nrw,ncl,false);
	imat ff(f.begin(),nrw,ncl,false);
	#ifdef _OPENMP
    #pragma omp parallel for
    #endif
  	for(int i=0;i<nrw;++i){
  		ff.row(i)=rOrder(xx.row(i),stable,descending);
  	}
  	return f;
}

RcppExport SEXP Rfast_row_order_p(SEXP xSEXP,SEXP stableSEXP,SEXP descendingSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    traits::input_parameter< const bool >::type descending(descendingSEXP);
    __result = wrap(row_order_p(x,stable,descending));
    return __result;
END_RCPP
}
