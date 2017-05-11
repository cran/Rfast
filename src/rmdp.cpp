//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
colvec rmdp(NumericMatrix Y,const int h,umat rnd,const int itertime) {
  const int n = Y.nrow();
  mat y(Y.begin(),n,Y.ncol(),false);
  mat ny,tmp,sama;
  int crit,l;
  double tempdet,bestdet=0;
  colvec jvec(n), ivec(n),final_vec(n),disa(n);
  uvec dist_perm(n),indextony(h);
  rowvec mu_t,var_t;
  for (int A=0;A<itertime;++A) {
    ny=y.rows(rnd.col(A));
    mu_t = mean(ny,0); 
    var_t = colvar_rmdp(ny);
    tmp=y.each_row() - mu_t;
    tmp=square(tmp);
    sama = tmp.each_row() / var_t;
    disa = sum(sama,1);
    for(l=0,crit=10;crit && l <= 15;) {
      l++;
      ivec.fill(0);
      dist_perm = Order_rmdp(disa);
      for(int j=0;j<h;++j){
        indextony[j]=dist_perm[j];
        ivec[dist_perm[j]]=1;
      }
      crit = sum( abs(ivec - jvec) );
      jvec = ivec;
      ny = y.rows(indextony);
      mu_t = mean(ny,0);
      var_t = var(ny,0,0);
      tmp=y.each_row() - mu_t;
      tmp=square(tmp);
      sama = tmp.each_row() / var_t;
      disa = sum(sama,1);
    }
    tempdet = prod(var_t);
    if(!bestdet || tempdet < bestdet) {
      bestdet = tempdet;
      final_vec = jvec;
    }
  }
  return final_vec;
}


RcppExport SEXP Rfast_rmdp(SEXP ySEXP,SEXP hSEXP,SEXP rndSEXP,SEXP itertimeSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type y(ySEXP);
    traits::input_parameter< const int >::type h(hSEXP);
    traits::input_parameter< umat >::type rnd(rndSEXP);
    traits::input_parameter< const int >::type itertime(itertimeSEXP);
    __result = wrap(rmdp(y,h,rnd,itertime));
    return __result;
END_RCPP
}
