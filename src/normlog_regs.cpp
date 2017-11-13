//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include <cmath>

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
NumericMatrix normlog_regs(NumericVector Y,NumericMatrix X, NumericMatrix BE,const  double con,
                           const double tol,const bool logged,const bool parallel,const int maxiters){
  int n = X.nrow();
  int D = X.ncol();
  mat x(X.begin(),n,D,false);
  vec y(Y.begin(),n,false);
  mat be(BE.begin(), D, 2, false);

  vec va(D);

  #ifdef _OPENMP
    #pragma omp parallel if(parallel)
    {
  #endif
    vec tmpX, aold(2), tmpX2, yhat, com, com2, anew(2), yminyhat, tmpVec(2);
    double a,b, dera, dera2, derb, derb2, derab, com3, sumcom2, divider;
    rowvec berow(2);
    int ij;
    #ifdef _OPENMP
      #pragma omp for
    #endif
    for(int j = 0; j<D; j++){
      tmpX = x.col(j);
      berow = be.row(j);
      aold[0] = berow[0];
      aold[1] = berow[1];
      tmpX2 = tmpX % tmpX;
      a = aold[0];
      b = aold[1];
      yhat = exp(a + b * tmpX);
      com = y % yhat;
      com2 = yhat % yhat;
      sumcom2 = sum(com2);
      com3 = sum(com2 % tmpX);
      dera = sum(com) - sumcom2;
      dera2 = dera - sumcom2;
      derb = sum(com % tmpX) - com3;
      derb2 = sum(com % tmpX2) - 2 * sum(com2 % tmpX2);
      derab = derb - com3;

      divider = dera2 * derb2 - derab *derab;
      tmpVec[0] = (derb2 * dera - derab * derb)/divider;
      tmpVec[1] = (-derab * dera + dera2 * derb)/divider;

      anew[0] = aold[0] - tmpVec[0];
      anew[1] = aold[1] - tmpVec[1];

      ij =2;
      while (ij++<maxiters && sum( abs(anew - aold) ) > tol ){
        aold = anew;
        a = aold[0];
        b = aold[1];
        yhat = exp(a + b * tmpX);
        com = y % yhat;
        com2 = yhat % yhat;
        sumcom2 = sum(com2);
        com3 = sum(com2 % tmpX);
        dera = sum(com) - sumcom2;
        dera2 = dera - sumcom2;
        derb = sum(com % tmpX) - com3;
        derb2 = sum(com % tmpX2) - 2*sum(com2 % tmpX2);
        derab = derb - com3;

        divider = dera2 * derb2 - derab *derab;
        tmpVec[0] = (derb2 * dera - derab * derb)/divider;
        tmpVec[1] = (-derab * dera + dera2 * derb)/divider;

        anew[0] = aold[0] - tmpVec[0];
        anew[1] = aold[1] - tmpVec[1];
      }
      yminyhat = y - yhat;
      va[j] = sum(yminyhat%yminyhat);
    }
  #ifdef _OPENMP
    }
  #endif

  NumericMatrix ret(D,2);
  #ifdef _OPENMP
    #pragma omp parallel for if(parallel)
  #endif
  for(int k=0; k<D; k++){
    ret(k,0) = -n * log(va(k)) + con;
    ret(k,1) = R::pchisq(ret(k,0), 1, false, logged);
  }

  return ret;
}


RcppExport SEXP Rfast_normlog_regs(SEXP YSEXP,SEXP XSEXP,SEXP BESEXP,SEXP conSEXP,SEXP tolSEXP,SEXP loggedSEXP,SEXP parallelSEXP,SEXP maxitersSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type Y(YSEXP);
    traits::input_parameter< NumericMatrix >::type X(XSEXP);
    traits::input_parameter< NumericMatrix >::type BE(BESEXP);
    traits::input_parameter< const double >::type con(conSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const bool >::type logged(loggedSEXP);
    traits::input_parameter< const bool >::type parallel(parallelSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    __result = wrap(normlog_regs(Y,X,BE,con,tol,logged,parallel,maxiters));
    return __result;
END_RCPP
}
