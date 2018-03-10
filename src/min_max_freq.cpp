
//Author: Manos Papadakis

#include <RcppArmadillo.h>

#include "templates.h"

using namespace Rcpp;
using namespace std;

using std::sort;
using std::remove_if;
using std::vector;

NumericVector min_freq_d(NumericVector x,const int na_rm){
  NumericVector xx=clone(x);
  const int n_1 = na_rm ? x.size() : remove_if(xx.begin(),xx.end(),R_IsNA)-xx.begin();
  int i,j=0;
  sort(xx.begin(),xx.begin() + n_1);
  if(!na_rm){
  	xx.push_back(0.0);
  }
  int times=0;
  double v=xx[j],mn_val=0.0;
  int mn_fr=INT_MAX;
  for(i=1;i<n_1;++i){
    if(v!=xx[i]){
      times=i-j;
      if(times<mn_fr){
        mn_fr=times;
        mn_val=v;
        if(times==1){
          break;
        }
      }
      j=i;
      v=xx[j];
    }
  }
  return NumericVector::create(_["value"]=mn_val,_["freq"]=mn_fr);
}


RcppExport SEXP Rfast_min_freq_d(SEXP xSEXP,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = wrap(min_freq_d(x,na_rm));
    return __result;
END_RCPP
}

IntegerVector min_freq_i(IntegerVector X,const bool na_rm){
  int sz;
  IntegerVector x;
  if(na_rm){
    x=clone(X);
    sz = remove_if(x.begin(),x.end(),R_IsNA)-x.begin();
  }else{
    x=X;
    sz=x.size();
  }
  int aa,szp=sz,szn=sz,count_neg=0,count_pos=0;
  vector<int> f(sz),ff,neg(sz);
  IntegerVector::iterator a=x.begin();
  vector<int>::iterator F=f.begin(),nn=neg.begin(),index;
  for(;a!=x.end();++a){
    aa=*a;
    if(aa<0){
      if(-aa>=szn){
        neg.resize(-aa+1);
        nn=neg.begin();
        szn=neg.size();
      }
      count_neg++;
      *(nn-aa)+=1;
    }else{
      if(aa>=szp){
        f.resize(aa+1);
        F=f.begin();
        szp=f.size();
      }
      count_pos++;
      *(F+aa)+=1;
    }
  }
  int val,freq;
  if(!count_neg){
    index=min_element(f.begin(),f.end());
    val=index-f.begin();
    freq=*index;
  }else if(!count_pos){
    index=min_element(neg.begin(),neg.end());
    val=index-f.begin();
    freq=*index;
  }else{
    vector<int>::iterator index_neg=min_element(neg.begin(),neg.end());
    index=min_element(f.begin(),f.end());
    if(*index>*index_neg){
      freq = *index;
      val = index-f.begin();
    }else{
      freq = *index_neg;
      val = index_neg-neg.begin();
    }
  }
  return IntegerVector::create(_["value"]=val,_["frequency"]=freq);
}

RcppExport SEXP Rfast_min_freq_i(SEXP xSEXP,SEXP na_rmSEXP){
  BEGIN_RCPP
  RObject __result;
  RNGScope __rngScope;
  traits::input_parameter< IntegerVector >::type x(xSEXP);
  traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
  __result = wrap(min_freq_i(x,na_rm));
  return __result;
  END_RCPP
}

NumericVector max_freq_d(NumericVector x,const int na_rm){
  NumericVector xx=clone(x);
  const int n_1 = na_rm ? x.size() : remove_if(xx.begin(),xx.end(),R_IsNA)-xx.begin();
  int i,j=0;
  sort(xx.begin(),xx.begin() + n_1);
  if(!na_rm){
  	xx.push_back(0.0);
  }
  int times=0;
  double v=xx[j],mx_val=0.0;
  int mx_fr=0;
  for(i=1;i<n_1;++i){
    if(v!=xx[i]){
      times=i-j;
      if(times>mx_fr){
        mx_fr=times;
        mx_val=v;
      }
      j=i;
      v=xx[j];
    }
  }
  return NumericVector::create(_["value"]=mx_val,_["freq"]=mx_fr);
}


RcppExport SEXP Rfast_max_freq_d(SEXP xSEXP,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = wrap(max_freq_d(x,na_rm));
    return __result;
END_RCPP
}

IntegerVector max_freq_i(IntegerVector X,const bool na_rm){
  int sz;
  IntegerVector x;
  if(na_rm){
    x=clone(X);
    sz = remove_if(x.begin(),x.end(),R_IsNA)-x.begin();
  }else{
    x=X;
    sz=x.size();
  }
  int aa,szp=sz,szn=sz,count_neg=0,count_pos=0;
  vector<int> f(sz),ff,neg(sz);
  IntegerVector::iterator a=x.begin();
  vector<int>::iterator F=f.begin(),nn=neg.begin(),index;
  for(;a!=x.end();++a){
    aa=*a;
    if(aa<0){
      if(-aa>=szn){
        neg.resize(-aa+1);
        nn=neg.begin();
        szn=neg.size();
      }
      count_neg++;
      *(nn-aa)+=1;
    }else{
      if(aa>=szp){
        f.resize(aa+1);
        F=f.begin();
        szp=f.size();
      }
      count_pos++;
      *(F+aa)+=1;
    }
  }
  int val,freq;
  if(!count_neg){
    index=max_element(f.begin(),f.end());
    val=index-f.begin();
    freq=*index;
  }else if(!count_pos){
    index=max_element(neg.begin(),neg.end());
    val=index-f.begin();
    freq=*index;
  }else{
    vector<int>::iterator index_neg=max_element(neg.begin(),neg.end());
    index=max_element(f.begin(),f.end());
    if(*index>*index_neg){
      freq = *index;
      val = index-f.begin();
    }else{
      freq = *index_neg;
      val = index_neg-neg.begin();
    }
  }
  return IntegerVector::create(_["value"]=val,_["frequency"]=freq);
}

RcppExport SEXP Rfast_max_freq_i(SEXP xSEXP,SEXP na_rmSEXP){
  BEGIN_RCPP
  RObject __result;
  RNGScope __rngScope;
  traits::input_parameter< IntegerVector >::type x(xSEXP);
  traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
  __result = wrap(max_freq_i(x,na_rm));
  return __result;
  END_RCPP
}
