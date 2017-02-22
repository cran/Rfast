//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <vector>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
vector<int> sort_unique_int(vector<int> x){
  bool count_neg=false,count_pos=false;
  int aa,szn=1,szp=1;
  vector<int> f(1,INT_MAX),ff,neg(1,INT_MAX);
  vector<int>::iterator a=x.begin(),F=f.begin(),nn=neg.begin();
  for(;a!=x.end();++a){
    aa=*a;
    if(aa<0){
      if(-aa>=szn){
        szn=-aa+1;
        neg.resize(szn,INT_MAX);
        nn=neg.begin();
      }
      count_neg=true;
      *(nn-aa)=aa;
    }else{
      if(aa>=szp){
        szp=aa+1;
        f.resize(szp,INT_MAX);
        F=f.begin();
      }
      count_pos=true;
      *(F+aa)=aa;
    }
  }
  if(count_neg){
    vector<int>::reverse_iterator nr;
    for(nr=neg.rbegin();nr!=neg.rend();++nr)
      if(*nr!=INT_MAX)
        ff.push_back(*nr);
  }
  if(count_pos)
    for(a=f.begin();a!=f.end();++a)
      if(*a!=INT_MAX)
        ff.push_back(*a);
  return ff;
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
