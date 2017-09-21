//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
static mat cross(mat x){
  const int ncl=x.n_cols,nrw=x.n_rows;
  mat f(ncl,ncl);
  colvec xv(nrw);
  double a;
  int i,j;
  for(i=0;i<ncl;++i){
    xv=x.col(i);
    for(j=i;j<ncl;++j){
      a=sum(x.col(j)%xv);
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}


//[[Rcpp::export]]
NumericMatrix regression(DataFrame x, NumericVector Y){
  const int size_F=x.length();
  colvec y(Y.begin(),Y.size(),false),cxx;
  NumericMatrix FF(2,size_F);
  mat F(FF.begin(),2,size_F,false);
  vector<int> pos=which_isFactor(x);
  const int num_of_facs=pos.size();
  DataFrame::iterator xx=x.begin();
  if(!num_of_facs){
    for(int i=0;i<size_F;++i,++xx){
      F.at(0,i)=regression_only_col(as<colvec>(*xx),y);
      F.at(1,i)=1;
    }
    return FF;
  }
  NumericMatrix strtonum_des;
  int pos_f=0,ptf=0, n=x.nrows(),p;
  mat tr_strtonum,b,strtonum;
  double SSO=var(y)*(double)(n-1),SS1;
  vec res;
  CharacterVector leksi;
  pos_f=pos[0]-1;
  for(int i=0;i<size_F;++i,++xx){
    if(pos_f==i){
      leksi=*xx;
      strtonum_des=design_matrix_regr(leksi);
      strtonum=mat(strtonum_des.begin(),strtonum_des.nrow(),strtonum_des.ncol(),false);
      tr_strtonum=strtonum.t();
      b=inv(cross(strtonum))*tr_strtonum*y;
      //b=solve( tr_strtonum*strtonum,tr_strtonum)*y;
      res=y-strtonum*b; 
      SS1=var(res)*(n-1);
      p=strtonum.n_cols;
      F.at(0,i)=(SSO-SS1)*(n-p)/((p-1)*SS1);
      F.at(1,i)=p-1;
      if(ptf!=num_of_facs-1)
        pos_f=pos[++ptf]-1;
    }else{
      NumericVector xxx=*xx;
      cxx=colvec(xxx.begin(),strtonum_des.nrow(),strtonum_des.ncol(),false);
      F.at(0,i)=regression_only_col(cxx,y);
      F.at(1,i)=1;
    }
  }
  return FF;
}

//regression
RcppExport SEXP Rfast_regression(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< DataFrame >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type y(ySEXP);
    __result = wrap(regression(x,y));
    return __result;
END_RCPP
}
