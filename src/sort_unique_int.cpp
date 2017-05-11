//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include "mn.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
vector<int> sort_unique_int(vector<int> x){
  int aa,mx,mn,count_not_zero=0;
  int count_pos=0;
  max_neg_pos(&x[0],&(*x.end()),mx,mn,count_pos);
  const int count_neg=x.size()-count_pos;
  vector<int> pos,f,neg;
  vector<int>::iterator a=x.begin(),pp,nn,F;
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
  f.resize(count_not_zero,INT_MAX);
  F=f.begin();
  if(count_neg){
    vector<int>::reverse_iterator nr=neg.rbegin();
    for(;nr!=neg.rend();++nr)
      if(*nr!=INT_MAX){
        *F++=*nr;
      }
  }
  if(count_pos)
    for(a=pos.begin();a!=pos.end();++a)
      if(*a!=INT_MAX){
        *F++=*a;
      }
  return f;
}

RcppExport SEXP Rfast_sort_unique_int(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<int> >::type x(xSEXP);
    __result = wrap(sort_unique_int(x));
    return __result;
END_RCPP
}
