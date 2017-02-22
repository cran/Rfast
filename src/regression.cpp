//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
mat regression(DataFrame x, colvec y){
  int size_F=x.length(),i;
  mat F(2,size_F);
  vector<int> pos=which_isFactor(x);
  DataFrame::iterator xx=x.begin();
  if(!pos.size()){
    for(i=0;i<size_F;++i,++xx){
      F.at(0,i)=regression_only_col(as<colvec>(*xx),y);
      F.at(1,i)=1;
    }
    return F;
  }
  NumericMatrix strtonum_des;
  int pos_f=0,ptf=0, n=x.nrows(),p;
  mat strtonum,tr_strtonum,b;
  double SSO=var(y)*(double)(n-1),SS1;
  vec res;
  CharacterVector leksi;
  pos_f=pos[ptf]-1;
  for(i=0;i<size_F;++i,++xx){
    if(pos_f==i){
      leksi=*xx;
      strtonum_des=design_matrix(leksi,true);
      strtonum=mat(strtonum_des.begin(),strtonum_des.nrow(),strtonum_des.ncol(),false);
      tr_strtonum=strtonum.t();
      b=inv(tr_strtonum*strtonum)*tr_strtonum*y; 
      res=y-strtonum*b; 
      SS1=var(res)*(n-1);
      p=strtonum.n_cols;
      F.at(0,i)=(SSO-SS1)*(n-p)/((p-1)*SS1);
      F.at(1,i)=p-1;
      ptf++;
      pos_f=pos[ptf]-1;
    }else{
      F.at(0,i)=regression_only_col(as<colvec>(*xx),y);
      F.at(1,i)=1;
    }
  }
  return F;
}

//regression
RcppExport SEXP Rfast_regression(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< DataFrame >::type x(xSEXP);
    traits::input_parameter< colvec >::type y(ySEXP);
    __result = wrap(regression(x,y));
    return __result;
END_RCPP
}
