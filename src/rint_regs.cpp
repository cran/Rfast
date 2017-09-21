//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include "mn.h"
#include <cmath>
#include "reg_lib.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
NumericMatrix rint_regs(NumericMatrix X, NumericVector Y, NumericVector id, const double tol, 
                 const bool logged,const bool parallel,const int maxiters){
  int n = X.nrow(), D = X.ncol(), idmx = get_max(id);
  mat x(X.begin(), n,D,false);
  vec y(Y.begin(),n,false);
  
  NumericVector tb = Tabulate(id,idmx);
  
  vec ni(tb.begin(), tb.size(),false);
  vec ni2 = ni%ni;
  colvec sy = group_sum2(y, id);
  vec my = sy/ni;
  double Sy = sum(sy);
  vec r = conv_to<vec>::from(cov(y,x));
  double mesi = Sy/n;
  vec xs = colsumsVec(x, parallel);
  vec xs2 = colsumsVec(x%x, parallel);
  vec vx = (xs2 - (xs%xs)/n)/(n - 1);
  vec b(D);
  b = r/vx;
  vec a(D);
  a = mesi - b % xs/n;
  mat be(D,2);
  be.col(0) = a;
  be.col(1) = b;
  vec stat(D);
  
  #ifdef _OPENMP
        #pragma omp parallel if(parallel)
        {
  #endif
    vec Xi(n), sxy(2), b1(2), tmpvec(n), tmpvec2(idmx), hi2(idmx),b2(2),B(2), mx(idmx);
    mat sx(idmx,2), temptcom(idmx,2), tcom(2,idmx), A(2,2);
    sx.col(0) = ni;
    sx.col(1) = ni;
    int ij;
    sxy(0) = Sy;
    sxy(1) = Sy;
    double S, d,down, se,seb;
    mat  xx(2,2);
    xx(0,0) = n;
    #ifdef _OPENMP
        #pragma omp for
    #endif
    for(int i = 0; i < D; i++) {
      Xi = x.col(i);
      xx(0,1) = xs[i];
      xx(1,0) = xs[i];
      xx(1,1) = xs2[i];
      sx.col(1) = group_sum2(Xi,id);
      sxy(1) = sum(Xi % y);
      mx = sx.col(1)/ni;
      b1[0] = be.row(i)[0];
      b1[1] = be.row(i)[1];
      tmpvec = y - b1(0) - b1(1) * Xi; 
      S = sum(tmpvec % tmpvec);
      tmpvec2 = my - b1(0) - b1(1) * mx;
      hi2 = tmpvec2 % tmpvec2;
      d = gold_rat2(n, ni, ni2, S, hi2, tol);
      
      temptcom.col(0) = sx.col(0)/(1+ ni * d);
      temptcom.col(1) = sx.col(1)/(1+ ni * d);
      tcom = temptcom.t();
      
      A = xx - d * tcom * sx;
      
      B = sxy - d * tcom * sy;
      
      down = A(0,0) * A(1,1) - A(0,1)*A(0,1);
      b2(0) = (A(1,1) * B(0) - A(0,1) * B(1))/down;
      b2(1) = (- A(0,1) * B(0) + A(0,0) * B(1))/down;
      ij = 2;
      while(ij++<maxiters && sum(abs(b2 - b1))>tol){
        b1 = b2;
        tmpvec = y - b1(0) - b1(1) * Xi;
        S = sum(tmpvec%tmpvec);
        tmpvec2 = my - b1(0) - b1(1) * mx;
        hi2 = tmpvec2 % tmpvec2;

        d = gold_rat2(n, ni, ni2, S, hi2, tol);
        temptcom.col(0) = sx.col(0)/(1+ ni * d);
        temptcom.col(1) = sx.col(1)/(1+ ni * d);
        tcom = temptcom.t();
        A = xx - d * tcom * sx;
        B = sxy - d * tcom * sy;
        down = A(0,0) * A(1,1) - A(0,1) * A(0,1);
        b2(0) = (A(1,1) * B(0) - A(0,1) * B(1))/down;
        b2(1) = (- A(0,1) * B(0) + A(0,0) * B(1))/down;
      }
      se = (S - d * sum(ni2 % hi2/ (1 + ni * d) ) )/n;
      seb = A(0,0) / down * se;
      stat(i) = b2(1)*b2(1)/ seb;
    }
#ifdef _OPENMP
        }
#endif
    
  NumericMatrix ret(D,2);
  
    #ifdef _OPENMP
        #pragma omp parallel for if(parallel)
    #endif
    for(int i = 0; i < D; i++){
      ret(i,0) = stat(i);
      ret(i,1) = R::pf(ret(i,0), 1, n-4, false, logged);
    }
  
  return ret;
}

RcppExport SEXP Rfast_rint_regs(SEXP XSEXP,SEXP YSEXP,SEXP idSEXP,SEXP tolSEXP,SEXP loggedSEXP,SEXP parallelSEXP,SEXP maxitersSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type X(XSEXP);
    traits::input_parameter< NumericVector >::type Y(YSEXP);
    traits::input_parameter< NumericVector >::type id(idSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const bool >::type logged(loggedSEXP);
    traits::input_parameter< const bool >::type parallel(parallelSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    __result = wrap(rint_regs(X,Y,id,tol,logged,parallel,maxiters));
    return __result;
END_RCPP
}
