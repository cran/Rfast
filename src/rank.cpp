
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "templates.h"

using namespace Rcpp;
using namespace arma;

using std::vector;

static void Rank_mean(NumericVector& x,NumericVector& f,const bool descend){
  const int n=x.size(),n_1=n+1;
  int i,j=0;
  NumericVector xx=clone(x);
  xx.push_back(0);
  vector<int> ind=Order_rank<vector<int>,NumericVector>(xx,descend,false,1,0);
  int k=0,m,times=0;
  double mn=0.0,v=xx[ind[j]];
  for(i=1;i<n_1;++i){
    if(v!=xx[ind[i]]){
      times=i-j;
      mn=(j+1+i)*0.5; //mn=mean(seq(j+1,i));
      for(k=j,m=0;m<times;++m)
        f[ind[k++]]=mn;
      j=i;
      v=xx[ind[j]];
    }
  }
}

static void Rank_max(NumericVector& x,NumericVector& f,const bool descend){
  const int n=x.size(),n_1=n+1;
  int i,j=0;
  NumericVector xx=clone(x);
  xx.push_back(0.0);
  vector<int> ind=Order_rank<vector<int>,NumericVector>(xx,descend,false,1,0);
  int k=0,m,times=0;
  double v=xx[ind[0]];
  for(i=1;i<n_1;++i){
    if(v!=xx[ind[i]]){
      times=i-j;
      for(k=j,m=0;m<times;++m)
        f[ind[k++]]=i;
      j=i;
      v=xx[ind[j]];
    }
  }
}

NumericVector Rank_max2(NumericVector& x,const bool descend){
  const int n=x.size();
  int i,j=n-1;
  NumericVector xx=clone(x),f(n);
  vector<int> ind=Order_rank<vector<int>,NumericVector>(xx,descend,false,1,0);
  double v=xx[ind[j]];
  f[ind[j]]=j+2;
  for(i=j-1;i>=0;--i){
    if(v!=xx[ind[i]]){
      j=i;
      v=xx[ind[j]];
    }
    f[ind[i]]=j+2;
  }
  return f;
}

static void Rank_min(NumericVector& x,NumericVector& f,const bool descend){
  const int n=x.size();
  int i,j=0;
  NumericVector xx=clone(x);
  vector<int> ind=Order_rank<vector<int>,NumericVector>(xx,descend,false,0,1);
  double v=xx[ind[j]];
  f[ind[0]]=1;
  for(i=1;i<n;++i){
    if(v!=xx[ind[i]]){
      j=i;
      v=xx[ind[j]];
    }
    f[ind[i]]=j+1;
  }
}

static void Rank_first(NumericVector& x,NumericVector& f,const bool descend,const bool stable){
  const int n=x.size();
  NumericVector xx=clone(x);
  vector<int> ind=Order_rank<vector<int>,NumericVector>(xx,descend,stable,0,0);
  for(int i=0;i<n;++i){
    f[ind[i]]=i+1;
  }
}

NumericVector Rank(NumericVector x,string method="average",const bool descend=false,const bool stable=false){
  NumericVector res(x.size());
  if(method == "average"){
    Rank_mean(x,res,descend);
  }else if(method == "min"){
    Rank_min(x,res,descend);
  }else if(method == "max"){
    Rank_max(x,res,descend);
  }else if(method == "first"){
    Rank_first(x,res,descend,stable);
  }else{
    stop("Error. Wrong method.");
  }
  return res;
}

RcppExport SEXP Rfast_rank(SEXP xSEXP,SEXP methodSEXP,SEXP descendSEXP,SEXP stableSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< string >::type method(methodSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    __result = wrap(Rank(x,method,descend,stable));
    return __result;
END_RCPP
}



#include "templates.h"
#include <RcppArmadilloExtensions/sample.h>

//[[Rcpp::export]]
NumericVector group_block(NumericVector x,vector<int> ina,NumericVector un_sample_ina,IntegerVector starts,string method="sample"){
    //NumericVector un_sample_ina=Rcpp::RcppArmadillo::sample(un_ina,un_ina.size(),true,NumericVector());
    IntegerVector ind=Order<IntegerVector,vector<int>>(ina,false,false,0);
    NumericVector f(x.size());
    NumericVector::iterator ff=f.begin();
    for(int i=0;i<un_sample_ina.size();++i){
        for(int j=0,k=starts(un_sample_ina(i)-1);j<i+1;++j,++k){
            *ff++=x(ina[ind(k)]-1);
        }
    }
    return f;
}
