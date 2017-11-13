
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

static ivec Order_rankc(colvec& x,const bool descend,const bool stable,const int n,const int k){
  ivec ind=linspace<ivec>(0,x.n_elem-k,x.n_elem+1-k);
  if(descend){
    auto descend_func = [&](int i,int j){return x[i]>x[j];};
    stable ? stable_sort(ind.begin(),ind.end()-n,descend_func) : sort(ind.begin(),ind.end()-n,descend_func);
  }else{
    auto func = [&](int i,int j){return x[i]<x[j];};
    stable ? stable_sort(ind.begin(),ind.end()-n,func) : sort(ind.begin(),ind.end()-n,func);
  }
  return ind;
}

static colvec rankc_mean(colvec x,const bool descend){
  const int n=x.size(),n_1=n+1;
  int i,j=0;
  ivec ind=Order_rankc(x,descend,false,1,0);
  int k=0,m,times=0;
  x.insert_rows(n,1,true);
  colvec f(n);
  double mn=0.0,v=x[ind[j]];
  for(i=1;i<n_1;++i){
    if(v!=x[ind[i]]){
      times=i-j;
      mn=(j+1+i)*0.5; //mn=mean(seq(j+1,i));
      for(k=j,m=0;m<times;++m)
        f[ind[k++]]=mn;
      j=i;
      v=x[ind[j]];
    }
  }
  return f;
}

static colvec rankc_max(colvec x,const bool descend){
  const int n=x.size(),n_1=n+1;
  int i,j=0;
  ivec ind=Order_rankc(x,descend,false,1,0);
  int k=0,m,times=0;
  x.insert_rows(n,1,true);
  colvec f(n);
  double v=x[ind[j]];
  for(i=1;i<n_1;++i){
    if(v!=x[ind[i]]){
      times=i-j;
      for(k=j,m=0;m<times;++m)
        f[ind[k++]]=i;
      j=i;
      v=x[ind[j]];
    }
  }
  return f;
}

static colvec rankc_min(colvec x,const bool descend){
  const int n=x.size();
  int i,j=0;
  ivec ind=Order_rankc(x,descend,false,0,1);
  x.insert_rows(n,1,true);
  colvec f(n);
  double v=x[ind[j]];
  f[ind[0]]=1;
  for(i=1;i<n;++i){
    if(v!=x[ind[i]]){
      j=i;
      v=x[ind[j]];
    }
    f[ind[i]]=j+1;
  }
  return f;
}
static colvec rankc_first(colvec x,const bool descend,const bool stable){
  const int n=x.n_elem;
  ivec ind=Order_rankc(x,descend,stable,0,1);
  colvec f(n);
  for(int i=0;i<n;++i){
    f[ind[i]]=i+1;
  }
  return f;
}

//[[Rcpp::export]]
NumericMatrix col_ranks_p(NumericMatrix x,string method,const bool descend,const bool stable){
  const int ncl=x.ncol(),nrw=x.nrow();
  NumericMatrix f(nrw,ncl);
  mat xx(x.begin(),nrw,ncl,false);
  mat ff(f.begin(),nrw,ncl,false);
  if(method == "average"){
  	#ifdef _OPENMP
	#pragma omp parallel for
	#endif
    for(int i=0;i<ncl;++i){
	    ff.col(i)=rankc_mean(xx.col(i),descend);
	}
  }else if(method == "min"){
	#ifdef _OPENMP
	#pragma omp parallel for
	#endif
    for(int i=0;i<ncl;++i){
	    ff.col(i)=rankc_min(xx.col(i),descend);
	}
  }else if(method == "max"){
	#ifdef _OPENMP
	#pragma omp parallel for
	#endif  	
    for(int i=0;i<ncl;++i){
	    ff.col(i)=rankc_max(xx.col(i),descend);
    }
	}else if(method == "first"){
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif    
    for(int i=0;i<ncl;++i){
      ff.col(i)=rankc_first(xx.col(i),descend,stable);
    }
  }else
    stop("Error. Wrong method.");  
  return f;
}


RcppExport SEXP Rfast_col_ranks_p(SEXP xSEXP,SEXP methodSEXP,SEXP descendSEXP,SEXP stableSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< string >::type method(methodSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    __result = wrap(col_ranks_p(x,method,descend,stable));
    return __result;
END_RCPP
}

static ivec Order_rankr(rowvec& x,const bool descend,const bool stable,const int n,const int k){
  ivec ind=linspace<ivec>(0,x.n_elem-k,x.n_elem+1-k);
  if(descend){
    auto descend_func = [&](int i,int j){return x[i]>x[j];};
    stable ? stable_sort(ind.begin(),ind.end()-n,descend_func) : sort(ind.begin(),ind.end(),descend_func);
  }else{
    auto func = [&](int i,int j){return x[i]<x[j];};
    stable ? stable_sort(ind.begin(),ind.end()-n,func) : sort(ind.begin(),ind.end(),func);
  }
  return ind;
}

static rowvec rankr_mean(rowvec x,const bool descend){
  const int n=x.size(),n_1=n+1;
  int i,j=0;
  ivec ind=Order_rankr(x,descend,false,1,0);
  int k=0,m,times=0;
  x.insert_cols(n,1,true);
  rowvec f(n);
  double mn=0.0,v=x[ind[j]];
  for(i=1;i<n_1;++i){
    if(v!=x[ind[i]]){
      times=i-j;
      mn=(j+1+i)*0.5; //mn=mean(seq(j+1,i));
      for(k=j,m=0;m<times;++m)
        f[ind[k++]]=mn;
      j=i;
      v=x[ind[j]];
    }
  }
  return f;
}

static rowvec rankr_max(rowvec x,const bool descend){
  const int n=x.size(),n_1=n+1;
  int i,j=0;
  ivec ind=Order_rankr(x,descend,false,1,0);
  int k=0,m,times=0;
  x.insert_cols(n,1,true);
  rowvec f(n);
  double v=x[ind[j]];
  for(i=1;i<n_1;++i){
    if(v!=x[ind[i]]){
      times=i-j;
      for(k=j,m=0;m<times;++m)
        f[ind[k++]]=i;
      j=i;
      v=x[ind[j]];
    }
  }
  return f;
}

static rowvec rankr_min(rowvec x,const bool descend){
  const int n=x.size();
  int i,j=0;
  ivec ind=Order_rankr(x,descend,false,0,1);
  x.insert_cols(n,1,true);
  rowvec f(n);
  double v=x[ind[j]];
  f[ind[0]]=1;
  for(i=1;i<n;++i){
    if(v!=x[ind[i]]){
      j=i;
      v=x[ind[j]];
    }
    f[ind[i]]=j+1;
  }
  return f;
}
static rowvec rankr_first(rowvec x,const bool descend,const bool stable){
  const int n=x.n_elem;
  ivec ind=Order_rankr(x,descend,stable,0,1);
  rowvec f(n);
  for(int i=0;i<n;++i){
    f[ind[i]]=i+1;
  }
  return f;
}

//[[Rcpp::export]]
NumericMatrix row_ranks_p(NumericMatrix x,string method,const bool descend,const bool stable){
  const int ncl=x.ncol(),nrw=x.nrow();
  NumericMatrix f(nrw,ncl);
  mat xx(x.begin(),nrw,ncl,false);
  mat ff(f.begin(),nrw,ncl,false);
  if(method == "average"){
    #ifdef _OPENMP
  #pragma omp parallel for
  #endif
    for(int i=0;i<nrw;++i){
      ff.row(i)=rankr_mean(xx.row(i),descend);
  }
  }else if(method == "min"){
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif
    for(int i=0;i<nrw;++i){
      ff.row(i)=rankr_min(xx.row(i),descend);
  }
  }else if(method == "max"){
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif    
    for(int i=0;i<nrw;++i){
      ff.row(i)=rankr_max(xx.row(i),descend);
    }
  }else if(method == "first"){
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif    
    for(int i=0;i<nrw;++i){
      ff.row(i)=rankr_first(xx.row(i),descend,stable);
    }
  }else
    stop("Error. Wrong method.");  
  return f;
}

RcppExport SEXP Rfast_row_ranks_p(SEXP xSEXP,SEXP methodSEXP,SEXP descendSEXP,SEXP stableSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< string >::type method(methodSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    __result = wrap(row_ranks_p(x,method,descend,stable));
    return __result;
END_RCPP
}
