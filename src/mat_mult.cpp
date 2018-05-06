
//Author: Manos Papadakis

#include <RcppArmadillo.h>

#ifdef _OPENMP
#include <omp.h>
#endif


using namespace Rcpp;
using namespace arma;


mat mat_mult_p(NumericMatrix X,NumericMatrix Y){
    const int n=X.ncol(),p=Y.ncol();
    mat f(n,p),x(X.begin(),X.nrow(),n,false),y(Y.begin(),Y.nrow(),p,false);
    colvec yi(y.n_rows);
    for(int i=0;i<p;++i){
        yi=y.col(i);
        #ifdef _OPENMP
        #pragma omp parallel for
        #endif
        for(int j=0;j<n;++j)
            f(j,i)=dot(x.col(j),yi);
    }
    return f;
}

RcppExport SEXP Rfast_mat_mult_p(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericMatrix >::type y(ySEXP);
    __result = wrap(mat_mult_p(x,y));
    return __result;
END_RCPP
}
