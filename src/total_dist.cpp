// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace std;
using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
double total_dist(NumericMatrix x){
	const int ncl=x.ncol(),nrw=x.nrow();
	mat xx(x.begin(),nrw,ncl,false);
	colvec xv(nrw);
	double a=0.0;
	int i,j;
	for(i=0;i<ncl-1;++i){
      xv=xx.col(i);
      for(j=i+1;j<ncl;++j){
        a+=std::sqrt(sum(square(xv-xx.col(j))));
      }
    }
  	return 2.0*a;
}

RcppExport SEXP Rfast_total_dist(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(total_dist(x));
    return __result;
END_RCPP
}
