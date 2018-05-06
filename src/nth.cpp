//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>
#include "mn.h"
using namespace Rcpp;
using std::nth_element;
using std::remove_if;


static int nth_index_simple(NumericVector& x,const int& elem,const bool& descend){
  IntegerVector ind=seq(1,x.size());
  descend ?
      nth_element(ind.begin(),ind.begin()+elem-1,ind.end(),[&](int i,int j){return x[i-1]>x[j-1];})
  :
      nth_element(ind.begin(),ind.begin()+elem-1,ind.end(),[&](int i,int j){return x[i-1]<x[j-1];});
  
  return ind[elem-1];
}

static double nth_simple(NumericVector& X,const int& elem,const bool& descend){
  NumericVector x=clone(X);
  descend ?
      nth_element(x.begin(),x.begin()+elem-1,x.end(),[&](double a,double b){return a>b;})
  :
      nth_element(x.begin(),x.begin()+elem-1,x.end());

  return x[elem-1];
}

static int nth_index_na_rm(NumericVector& X,const int& elem,const bool& descend){
  	NumericVector x=clone(X);
  	const int new_end=remove_if(x.begin(),x.end(),R_IsNA)-x.begin();
	  IntegerVector ind= seq(1,new_end);
	  descend ?
	  nth_element(ind.begin(),ind.begin()+((elem<new_end) ? elem-1-new_end : elem-1),ind.end(),[&](int i,int j){return x[i-1]>x[j-1];})
	  :
	  nth_element(ind.begin(),ind.begin()+((elem<new_end) ? elem-1-new_end : elem-1),ind.end(),[&](int i,int j){return x[i-1]<x[j-1];});
	
  	return ind[elem-1];
}

static double nth_na_rm(NumericVector& X,const int& elem,const bool& descend){
  	NumericVector x=clone(X);
  	const int new_end=remove_if(x.begin(),x.end(),R_IsNA)-x.begin();
	  descend ?
	  nth_element(x.begin(),x.begin()+((elem<new_end) ? elem-1-new_end : elem-1),x.end(),[&](double a,double b){return a>b;})
	  :
	  nth_element(x.begin(),x.begin()+((elem<new_end) ? elem-1-new_end : elem-1),x.end());
	
  	return x[elem-1];
}

int nth_int(vector<int> x,int elem){
    int aa,mx,mn;
    bool has_pos=false,has_neg=false;
    max_neg_pos(&x[0],&x[x.size()-1]+1,mx,mn,has_pos,has_neg);
    vector<int> pos,f(x.size()),neg;
    vector<int>::iterator a=x.begin();
    vector<int>::reverse_iterator F=f.rbegin();
    if(has_pos){
        pos.resize(mx+1,0);
    }
    if(has_neg){
        neg.resize(1-mn,0);
    }
    if(has_pos && has_neg){
        for(;a!=x.end();++a){
            aa=*a;
            aa<0 ? neg[-aa]++ : pos[aa]++;
        }
    }else if(has_pos){
        for(;a!=x.end();++a){
            aa=*a;
            pos[aa]++;
        }
    }else{ 
        for(;a!=x.end();++a){
            aa=*a;
            neg[-aa]++;
        }
    }
    
    --elem;
    int res=0,num=0;
    if(has_neg){
        for(vector<int>::reverse_iterator nr=neg.rbegin();nr!=neg.rend();++nr){
            if(*nr!=0){
                num+=*nr;
                if(elem>num)
                    res=nr-neg.rbegin()+1;        
            }
        }
    }
    if(has_pos){
        for(a=pos.begin();a!=pos.end();++a){
            if(*a!=0){
                num+=*a;
                if(elem>num)
                    res=a-pos.begin()+1;
            }
        }
    }
    return res;
}

double nth(NumericVector x,const int elem,const bool descend,const bool na_rm){
	return na_rm ? nth_na_rm(x,elem,descend) : nth_simple(x,elem,descend);
}

int nth_index(NumericVector x,const int elem,const bool descend,const bool na_rm){
	return na_rm ? nth_index_na_rm(x,elem,descend) : nth_index_simple(x,elem,descend);
}

// nth
RcppExport SEXP Rfast_nth(SEXP xSEXP,SEXP elemSEXP,SEXP descendSEXP,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< const int >::type elem(elemSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = nth(x,elem,descend,na_rm);
    return __result;
END_RCPP
}

// nth_int
RcppExport SEXP Rfast_nth_int(SEXP xSEXP,SEXP elemSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<int> >::type x(xSEXP);
    traits::input_parameter< int >::type elem(elemSEXP);
    __result = nth_int(x,elem);
    return __result;
END_RCPP
}

// nth_index
RcppExport SEXP Rfast_nth_index(SEXP xSEXP,SEXP elemSEXP,SEXP descendSEXP,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< const int >::type elem(elemSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = wrap(nth_index(x,elem,descend,na_rm));
    return __result;
END_RCPP
}
