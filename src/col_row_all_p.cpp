
//Author: Manos Papadakis

#include <RcppArmadillo.h>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace arma;
using namespace std;


//[[Rcpp::export]]
SEXP col_all_p(LogicalMatrix x){
	const int n=x.ncol();
	SEXP f=PROTECT(Rf_allocVector(LGLSXP,n));
	imat xx(x.begin(),x.nrow(),n,false);
	int *ff=LOGICAL(x);
	#ifdef _OPENMP
    #pragma omp parallel for
  	#endif
	for(int i=0;i<n;++i){
		ff[i]=all(xx.col(i));
	}
	UNPROTECT(1);
	return f;
}

RcppExport SEXP Rfast_col_all_p(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< LogicalMatrix >::type x(xSEXP);
    __result = col_all_p(x);
    return __result;
END_RCPP
}

//[[Rcpp::export]]
SEXP row_all_p(LogicalMatrix x){
	const int n=x.nrow();
	SEXP f=PROTECT(Rf_allocVector(LGLSXP,n));
	imat xx(x.begin(),n,x.ncol(),false);
	int *ff=LOGICAL(x);
	#ifdef _OPENMP
    #pragma omp parallel for
  	#endif
	for(int i=0;i<n;++i){
		ff[i]=all(xx.row(i));
	}
	UNPROTECT(1);
	return f;
}

RcppExport SEXP Rfast_row_all_p(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< LogicalMatrix >::type x(xSEXP);
    __result = row_all_p(x);
    return __result;
END_RCPP
}
