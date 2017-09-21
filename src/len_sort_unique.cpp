//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include "mn.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
int len_sort_unique_int(IntegerVector x){
  int aa,mx,mn,count_not_zero=0;
  int count_pos=0;
  max_neg_pos(&x[0],&(*x.end()),mx,mn,count_pos);
  const int count_neg=x.size()-count_pos;
  vector<int> pos,f,neg;
  vector<int>::iterator pp,nn,F;
  IntegerVector::iterator a=x.begin();
  if(count_pos>0)
    pos.resize(mx+1,INT_MAX);
  if(count_neg>0)
    neg.resize(1-mn,INT_MAX);
  if(count_pos && count_neg){
    for(nn=neg.begin(),pp=pos.begin();a!=x.end();++a){
      aa=*a;
      if(aa<0)
        *(nn-aa)=aa;
      else
        *(pp+aa)=aa;
    }
  }else if(count_pos){
    for(pp=pos.begin();a!=x.end();++a){
      aa=*a;
      *(pp+aa)=aa;
    }
  }else{ 
    for(nn=neg.begin();a!=x.end();++a){
      aa=*a;
      *(nn-aa)=aa;
    }
  }
  if(count_neg)
    for(nn=neg.begin();nn!=neg.end();++nn)
      if(*nn!=INT_MAX)
        count_not_zero++;
  if(count_pos)
    for(pp=pos.begin();pp!=pos.end();++pp)
      if(*pp!=INT_MAX)
        count_not_zero++;
  return count_not_zero;
}


//[[Rcpp::export]]
int len_sort_unique_double(vector<double> x){
  sort(x.begin(),x.end());
  x.erase( unique( x.begin(), x.end() ), x.end() );
  return x.size();
}

RcppExport SEXP Rfast_len_sort_unique_double(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<double> >::type x(xSEXP);
    __result = wrap(len_sort_unique_double(x));
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_len_sort_unique_int(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< IntegerVector >::type x(xSEXP);
    __result = wrap(len_sort_unique_int(x));
    return __result;
END_RCPP
}
